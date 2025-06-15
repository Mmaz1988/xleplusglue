import shutil
import os
import subprocess
import re
import logging

logging.basicConfig(level=logging.DEBUG, format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger(__name__)

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
        'info_neg_check': '{}(info_pos_check, axiom, (({}) => ({}))).\n',
        'info_pos_check': '{}(info_neg_check, axiom, ~(({}) => ({}))).\n',
        'cons_pos_check': '{}(cons_pos_check, axiom, ({} & {})).\n',
        'cons_neg_check': '{}(cons_neg_check, axiom, ({}) => ~({})).\n'
    }
    q = context
    p = hypothesis
    # Generate and write TPTP files for each template
    for suffix, template in templates.items():
        tptp_content = ""
        # read in axioms_file
        tptp_content += f"{axioms}\n\n"

        tptp_content += f"% p = {q}\n% q = {p}\n"  # Add comments with p and q
        tptp_content += template.format(logic,q,p)
        filename = f"sem_{suffix}.p"
        file_path = os.path.join(output_folder, filename)
        logging.debug(f"Writing TPTP file with content:\n{tptp_content}\n")
        with open(file_path, mode='w') as file:
            file.write(tptp_content)


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

def bloodsuck(file_path, mode=["-sa", "fmb"], timeout=7,vampire_path="bin"):
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
    result = {
        "Filename": filename,
        "Termination Reason": "Unknown",
        "Termination Phase": "Unknown",
        "Finite Model Found": "Unknown",
        "SZS Status": "Unknown"
    }

    # Construct the Vampire command
    vampire_path = os.path.join(vampire_path, "vampire")
    command = [str(vampire_path), file_path, "-t", str(timeout)] + mode

    logging.debug("Executing: %s", " ".join(command))
    # print("Executing: ", " ".join(command), "\r", flush=True)

    try:
        # Run Vampire with timeout
        completed_process = subprocess.run(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=timeout
        )

        # Extract information from the output
        output = completed_process.stdout
        logging.debug("Vampire Output: %s", output)
        # print(output)
        termination_reason, termination_phase, finite_model_found, szs_status = extract_vampire_info(output)

        # Update the result dictionary
        result.update({
            "Termination Reason": termination_reason,
            "Termination Phase": termination_phase,
            "Finite Model Found": finite_model_found,
            "SZS Status": szs_status
        })

    except subprocess.TimeoutExpired:
        result["Termination Reason"] = "Timeout"
    except Exception as e:
        result["Termination Reason"] = f"Error: {e}"

    return result

def massacer(folder_path, mode=["-sa", "fmb"], timeout=7,vampire_path ="bin"):
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
        print(f"Error: Folder '{folder_path}' does not exist.")

    file_list = sorted(os.listdir(folder_path))
    # Iterate over all .p files in the folder
    for i,filename in enumerate(file_list):
        if filename.endswith(".p"):
            # print file being analyzed and flush stdout to see progress
            print(f"Analyzing {filename}...({i+1}/{len(file_list)})", end="\r", flush=True)
            file_path = os.path.join(folder_path, filename)
            result = bloodsuck(file_path, mode, timeout, vampire_path)
            results.append(result)

    # delete all files
    shutil.rmtree("tmp/current")

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
        "Timeout": ("●", "yellow", 18),  # Larger circle for neutral
        "Unknown": ("●", "yellow", 18),  # Larger circle for neutral
        "Unsatisfiable": ("▼", "red", 16),  # Downward triangle for negative
        "True": ("▲", "green", 16),  # Upward triangle for positive
        "False": ("▼", "red", 16)  # Downward triangle for negative
    }

    # Define property order (ignoring Termination Phase)
    property_labels = ["Termination Reason", "Finite Model Found", "SZS Status"]

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
    informative = determine_informativity(informativity_check)

    return consistent, informative


"""
Determines consistency based on mapped values.
"""
def determine_consistency(data):
    # Placeholder: Implement specific consistency conditions
    logger.debug("Consistency Check: %s", data)

    failed_pos_check = sum(1 for value in data["pos"] if value == -1) > len(data["neg"]) / 2

    if failed_pos_check:
        return False

    return True


"""
Determines informativity based on mapped values.
"""
def determine_informativity(data):
    logger.debug("Informativity Check: %s", data)
    # Placeholder: Implement specific informativity conditions
    successful_neg_check = sum(1 for value in data["neg"] if value == -1) > len(data["neg"]) / 2

    if successful_neg_check:
        return False

    successful_pos_check = sum(1 for value in data["pos"] if value == 1) > len(data["pos"]) / 2
    if successful_pos_check:
        return True

    return False

