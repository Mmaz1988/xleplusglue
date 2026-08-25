import shutil
import os
import subprocess
import re
import logging
import time
import traceback

# Level and handlers are configured once in logging_config, from the entrypoint.
logger = logging.getLogger(__name__)

# Same flag/env var as run_vampire.KEEP_TPTP_FILES -- read independently here rather than
# imported, since this module has no other dependency on run_vampire. When set, massacer()
# leaves each folder's .p files on disk instead of deleting them right after running Vampire.
KEEP_TPTP_FILES = os.getenv("VAMPIRE_KEEP_TPTP", "false").strip().lower() in ("1", "true", "yes", "on")

# Marker for the block append_result_summary() writes at the end of each .p file. Also used
# to strip a previous run's block, so re-running Vampire over the same file (the fmb -> casc
# retry in run_vampire.run_vampire_batch) replaces the summary instead of stacking them.
SUMMARY_MARKER = "% ---- Vampire run ----"


def comment_line(value):
    """Flattens a value so it is safe on a single TPTP `%` comment line."""
    return " ".join(str(value).split())


def build_vampire_command(file_path, mode, timeout, vampire_path="bin"):
    """The exact argv Vampire is invoked with -- one definition, so the command recorded in
    the .p file summary cannot drift from the command that was actually run."""
    return [str(os.path.join(vampire_path, "vampire")), str(file_path), "-t", str(timeout)] + list(mode)


def append_result_summary(file_path, result, timeout, elapsed=None):
    """Appends how Vampire was called and what it answered to the .p file, as TPTP comments.

    The .p files under tmp/ are kept for debugging (VAMPIRE_KEEP_TPTP); on their own they say
    nothing about the run they came from, so the verdict and the invocation are recorded next
    to the formulas that produced them.
    """
    if not os.path.isfile(file_path):
        return
    lines = [
        SUMMARY_MARKER,
        f"% command: {comment_line(' '.join(result.get('Command') or []) or 'Unknown')}",
        f"% time limit: {comment_line(timeout)}s"
        + (f" (returned after {elapsed:.2f}s)" if elapsed is not None else ""),
        f"% SZS status: {comment_line(result.get('SZS Status'))}",
        f"% termination reason: {comment_line(result.get('Termination Reason'))}",
        f"% termination phase: {comment_line(result.get('Termination Phase'))}",
        f"% finite model found: {comment_line(result.get('Finite Model Found'))}",
    ]
    try:
        with open(file_path) as file:
            content = file.read()
        marker_at = content.find(SUMMARY_MARKER)
        if marker_at != -1:
            content = content[:marker_at]
        with open(file_path, mode="w") as file:
            file.write(content.rstrip("\n") + "\n\n" + "\n".join(lines) + "\n")
    except OSError as error:
        # A summary is debugging sugar -- never let it take down a Vampire run.
        logger.warning("Could not write the run summary into %s: %s", file_path, error)


def generate_tptp_files(context, hypothesis, axioms="", logic="fof", output_folder = "tmp/current/"):
    """
    Generates TPTP files from a CSV file containing formulas p and q, including comments with the original formulas.

    Parameters:
    - csv_path (str): Path to the input CSV file with columns 'id', 'p', and 'q'.
    - output_folder (str): Directory where the TPTP files will be saved.
    - separator (str): The delimiter used in the CSV file (default is ';').
    """
    # Ensure the output directory exists
    os.makedirs(output_folder, exist_ok=True)

    # Define TPTP templates with placeholders for p and q
    templates = {
        'info_pos_check': '{}(info_pos_check, axiom, ({q}) & ~(({q}) => ({p}))).\n',
        'info_neg_check': '{}(info_neg_check, axiom, ({q}) & (({q}) => ({p}))).\n',
        'cons_pos_check': '{}(cons_pos_check, axiom, ({q} & {p})).\n',
        'cons_neg_check': '{}(cons_neg_check, axiom, ({q}) & ~(({q}) => ~({p}))).\n'
    }
    q = context
    p = hypothesis
    # Generate and write TPTP files for each template
    files = []
    for suffix, template in templates.items():
        tptp_content = ""
        # read in axioms_file
        tptp_content += f"{axioms}\n\n"

        # The templates below bind q to the context and p to the hypothesis, so label
        # them that way round -- these comments used to print the context under "p =".
        tptp_content += f"% check = {suffix}\n"
        tptp_content += f"% q (context) = {comment_line(q)}\n"
        tptp_content += f"% p (hypothesis) = {comment_line(p)}\n"
        tptp_content += template.format(logic,q=q,p=p)
        filename = f"sem_{suffix}.p"
        file_path = os.path.join(output_folder, filename)
        logger.debug("Writing TPTP file %s with content:\n%s\n", file_path, tptp_content)

        files.append(tptp_content)

        with open(file_path, mode='w') as file:
            file.write(tptp_content)

    return files

def extract_vampire_info(output):
    """
    Extracts the termination reason, termination phase, whether a finite model was found, and the SZS status from Vampire's output.

    Parameters:
    - output (str): The standard output from Vampire.

    Returns:
    - termination_reason (str): The reason why Vampire stopped (e.g., Satisfiable, Refutation, Time limit, etc.).
    - termination_phase (str): The phase in which Vampire terminated (e.g., Saturation, Preprocessing).
    - finite_model_found (str): "True" if a finite model was found, "False" if explicitly stated otherwise, "Unknown" if no information is provided.
    - szs_status (str): The SZS status (e.g., "Satisfiable", "Theorem", "CounterSatisfiable", "Timeout").
    """
    # Extract the last termination reason
    termination_matches = re.findall(r"% Termination reason: (.+)", output)
    termination_reason = termination_matches[-1].strip() if termination_matches else "Unknown"

    # Terminantion reason refutation not found
    if "Refutation not found" in termination_reason:
        termination_reason = "Refutation not found"

    # Extract the last termination phase (if available)
    termination_phase_matches = re.findall(r"% Termination phase: (.+)", output)
    termination_phase = termination_phase_matches[-1].strip() if termination_phase_matches else "Unknown"

    # Extract all finite model mentions and take the last relevant occurrence
    finite_model_matches = re.findall(r"(Finite Model Found!|finite model not found)", output)
    if finite_model_matches:
        last_mention = finite_model_matches[-1]
        finite_model_found = "True" if last_mention == "Finite Model Found!" else "False"
    else:
        finite_model_found = "Unknown"

    # Extract the last SZS status
    szs_matches = re.findall(r"% SZS status (\w+) for", output)
    szs_status = szs_matches[-1].strip() if szs_matches else "Unknown"

    return termination_reason, termination_phase, finite_model_found, szs_status

def bloodsuck(file_path, mode=["-sa", "fmb"], timeout=15,vampire_path="bin"):
    """
    Runs Vampire theorem prover on a single .p file and extracts relevant information.

    Parameters:
    - file_path (str): Path to the .p file.
    - mode (list): List of additional Vampire mode arguments.
    - timeout (int): Timeout in seconds for the Vampire process.

    Returns:
    - result (dict): A dictionary containing the extracted information.
    """
    filename = os.path.basename(file_path)
    command = build_vampire_command(file_path, mode, timeout, vampire_path)
    result = {
        "Filename": filename,
        # The invocation itself, so the summary written into the .p file (and any caller
        # reporting a verdict) can say which Vampire mode produced it.
        "Command": command,
        "Termination Reason": "Unknown",
        "Termination Phase": "Unknown",
        "Finite Model Found": "Unknown",
        "SZS Status": "Unknown"
    }

    logger.debug("Executing: %s", " ".join(command))
    # print("Executing: ", " ".join(command), "\r", flush=True)

    started_at = time.monotonic()
    try:
        # Run Vampire with timeout
        completed_process = subprocess.run(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=timeout
        )
        #Logg exit code
        logger.debug("Vampire exited with code: %s", completed_process.returncode)

        # Extract information from the output
        output = completed_process.stdout
        # print(f"Vampire Output: {output}")
        # print(f"Error output: {completed_process.stderr}")
        # logger.info("Vampire Output: %s" + output)
        # logger.debug("Error output: %s" + completed_process.stderr)
        # print(output)
        termination_reason, termination_phase, finite_model_found, szs_status = extract_vampire_info(output)

        # Update the result dictionary
        result.update({
            "Termination Reason": termination_reason,
            "Termination Phase": termination_phase,
            "Finite Model Found": finite_model_found,
            "SZS Status": szs_status
        })

    except subprocess.TimeoutExpired as e:
        logger.warning("Vampire process timed out after %d seconds", timeout)
        result["Termination Reason"] = "Timeout"
        result["Termination Phase"] = "Timeout"
        result["Finite Model Found"] = "Unknown"
        result["SZS Status"] = "Timeout"
        if e.stdout:
            logger.warning("Partial STDOUT before timeout:\n%s", e.stdout)
        if e.stderr:
            logger.warning("Partial STDERR before timeout:\n%s", e.stderr)
    except Exception as e:
        #print stacktrace
        logger.error("An error occurred while running Vampire: %s", e)
        logger.error("Stacktrace: %s", traceback.format_exc())

        result["Termination Reason"] = f"Error: {e}"

    append_result_summary(file_path, result, timeout, elapsed=time.monotonic() - started_at)

    return result

def massacer(folder_path, mode=["-sa", "fmb"], timeout=15,vampire_path ="bin"):
    """
    Processes all .p files in the given folder using the Vampire theorem prover.

    Parameters:
    - folder_path (str): Path to the folder containing .p files.
    - mode (list): List of additional Vampire mode arguments.
    - timeout (int): Timeout in seconds for each Vampire process.

    Returns:
    - results_df (pd.DataFrame): A Pandas DataFrame containing the results.
    """
    results = []

    # Ensure the folder exists
    if not os.path.isdir(folder_path):
        logger.error("Folder '%s' does not exist.", folder_path)

    file_list = sorted(os.listdir(folder_path))
    # Iterate over all .p files in the folder
    for i,filename in enumerate(file_list):
        if filename.endswith(".p"):
            # print file being analyzed and flush stdout to see progress
            # print(f"Analyzing {filename}...({i+1}/{len(file_list)})", end="\r", flush=True)
            file_path = os.path.join(folder_path, filename)
            result = bloodsuck(file_path, mode, timeout, vampire_path)
            results.append(result)

    # delete all files from the run-specific output folder
    if not KEEP_TPTP_FILES:
        shutil.rmtree(folder_path, ignore_errors=True)

    return results


def generate_svg_glyph(data):
    """
    Generates a compact inline SVG glyph with minimal whitespace.
    Each row corresponds to one check, and each column represents:
    - Termination Reason
    - Finite Model Found
    - SZS Status

    Enhancements:
    - Superscript `+` or `-` at the end of each row, slightly shifted right.
    - A dividing line, slightly lower, separating Consistency and Informativity checks.
    - Labels "C" (Consistency) and "I" (Informativity) directly above and below the line.
    - Larger yellow circle for better visibility.
    - Added padding around the whole SVG.
    """

    # Define mapping for symbols and colors
    symbol_map = {
        "Satisfiable": ("▲", "green", 16),  # Upward triangle for positive
        "Refutation": ("▼", "red", 16),  # Downward triangle for negative
        "Refutation not found": ("●", "yellow", 18),
        "Unknown": ("●", "yellow", 18),  # Larger circle for neutral
        "Unsatisfiable": ("▼", "red", 16),  # Downward triangle for negative
        "True": ("▲", "green", 16),  # Upward triangle for positive
        "False": ("▼", "red", 16),  # Downward triangle for negative
        "Timeout": ("■", "yellow", 16),
        "Time limit": ("■", "yellow", 16)  # Square for timelimit
    }

    # Define property order (ignoring Termination Phase)
    property_labels = ["Termination Reason", "SZS Status", "Finite Model Found"]

    # Define check categories (first two are Consistency, last two are Informativity)
    is_positive_check = [False, True, False, True]  # False = negative check, True = positive check

    # SVG parameters (compact size with padding)
    padding = 5  # Extra padding around the SVG
    symbol_size = 16  # Standard font size for symbols
    row_spacing, col_spacing = 18, 18  # Adjusted spacing
    superscript_size = 10  # Smaller size for + / -
    superscript_x_offset = 18  # Slightly shift right

    num_rows = len(data)
    num_cols = len(property_labels)
    width = col_spacing * num_cols + 34 + padding * 2  # Extra space for superscripts and padding
    height = row_spacing * num_rows + padding * 2

    # SVG header
    svg = f'<svg width="{width}" height="{height}" xmlns="http://www.w3.org/2000/svg">'

    # Loop over each check (row)
    for i, check in enumerate(data):
        for j, prop in enumerate(property_labels):  # Only iterate over relevant properties
            value = check[prop]
            symbol, color, size = symbol_map.get(value, ("?", "black", symbol_size))  # Default to '?' if unknown

            x = j * col_spacing + (col_spacing // 2) + 15 + padding  # Center symbol in column, shift right
            y = i * row_spacing + (row_spacing // 2) + padding  # Center symbol in row

            # Append SVG text element with thicker black stroke (border)
            svg += f'<text x="{x}" y="{y}" font-size="{size}" fill="{color}" stroke="black" stroke-width="1.5" text-anchor="middle" dominant-baseline="middle">{symbol}</text>'

        # Add superscript (+ or -) slightly shifted right
        superscript = "+" if is_positive_check[i] else "-"
        svg += f'<text x="{width - superscript_x_offset - padding}" y="{y - 3}" font-size="{superscript_size}" fill="black" text-anchor="middle">{superscript}</text>'

    # Draw dividing line slightly lower between Consistency and Informativity checks
    divider_y = row_spacing * 2 + padding - 2  # Adjusted lower so it does not cross symbols
    svg += f'<line x1="{padding}" y1="{divider_y}" x2="{width - padding - 10}" y2="{divider_y}" stroke="black" stroke-width="1"/>'

    # Draw vertical dashed divider after the first two columns
    vertical_divider_x = padding + col_spacing * 2 + 15  # 15 matches the x-offset you use for symbols
    svg += (
            f'<line x1="{vertical_divider_x}" y1="{padding}" '
            f'x2="{vertical_divider_x}" y2="{height - padding}" '
            f'stroke="black" stroke-width="1" stroke-dasharray="4,2"/>'
            )

    # Add "C" (Consistency) and "I" (Informativity) labels directly above and below the line
    svg += f'<text x="{padding + 5}" y="{divider_y - 3}" font-size="{symbol_size}" fill="black" font-weight="bold" text-anchor="middle">C</text>'
    svg += f'<text x="{padding + 5}" y="{divider_y + 14}" font-size="{symbol_size}" fill="black" font-weight="bold" text-anchor="middle">I</text>'

    # Close SVG
    svg += "</svg>"

    return svg


def discourse_checks(data):
    """
    Processes theorem prover results and maps values into a numerical representation.

    Mapping:
    - 1  → "Satisfiable" / "True"
    - 0  → "Unknown"
    - -1 → "Unsatisfiable" / "False"
    - -9 → Anything else (unexpected)

    Returns:
    - Two booleans: (consistent, informative)
    """

    # Define mapping for values
    value_map = {
        "Satisfiable": 1,
        "Unsatisfiable": -1,
        "Refutation": -1,
        "True": 1,
        "False": -1,
        "Unknown": 0,
        "Refutation not found": 0,
        "Time limit": 0,
        "Timeout": 0  # Timeout is treated as unknown
    }

    consistency_check = {}
    informativity_check = {}

    for check in data:
        # Extract values and map them
        mapped_values = [
            value_map.get(check["Termination Reason"], -9),
            value_map.get(check["Finite Model Found"], -9),
            value_map.get(check["SZS Status"], -9)
        ]

        # Assign to the corresponding check
        if check["Filename"].startswith("sem_cons_pos_check"):
            consistency_check["pos"] = mapped_values
        elif check["Filename"].startswith("sem_cons_neg_check"):
            consistency_check["neg"] = mapped_values
        elif check["Filename"].startswith("sem_info_pos_check"):
            informativity_check["pos"] = mapped_values
        elif check["Filename"].startswith("sem_info_neg_check"):
            informativity_check["neg"] = mapped_values

    # Determine consistency and informativity
    consistent = determine_consistency(consistency_check)
    informative, maxim_of_relevance  = determine_informativity(informativity_check)

    return consistent, informative, maxim_of_relevance


"""
Determines consistency based on mapped values.
"""
def determine_consistency(data):
    # Placeholder: Implement specific consistency conditions
    logger.debug("Consistency Check: %s", data)

    failed_pos_check = sum(1 for value in data["pos"] if value == -1) >= 2
    successful_neg_check = sum(1 for value in data["neg"] if value == -1) >= 2

    if failed_pos_check and successful_neg_check:
        return False

    return True


"""
Determines informativity based on mapped values.
"""
def determine_informativity(data):
    logger.debug("Informativity Check: %s", data)
    # Placeholder: Implement specific informativity conditions

    if sum(data["neg"]) == 0 and sum(data["pos"]) == 0:
        logger.debug("Assuming maxime of relevance for informativity")
        return True, True  # Maxime of relevance is assumed to be true

    successful_neg_check = sum(1 for value in data["neg"] if value == -1) > len(data["neg"]) / 2

    if successful_neg_check:
        return False, False

    successful_pos_check = sum(1 for value in data["pos"] if value == 1) > len(data["pos"]) / 2
    if successful_pos_check:
        return True, False

    return False, False
