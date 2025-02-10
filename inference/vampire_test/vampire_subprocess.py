import os
import subprocess
import pandas as pd
import re


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
    # Extract termination reason
    termination_match = re.search(r"% Termination reason: (.+)", output)
    termination_reason = termination_match.group(1).strip() if termination_match else "Unknown"

    # Extract termination phase (if available)
    termination_phase_match = re.search(r"% Termination phase: (.+)", output)
    termination_phase = termination_phase_match.group(1).strip() if termination_phase_match else "Unknown"

    # Check for explicit mention of finite model presence or absence
    if "Finite Model Found!" in output:
        finite_model_found = "True"
    elif "finite model not found" in output:
        finite_model_found = "False"
    else:
        finite_model_found = "Unknown"

    # Extract SZS status
    szs_match = re.search(r"% SZS status (\w+) for", output)
    szs_status = szs_match.group(1).strip() if szs_match else "Unknown"

    return termination_reason, termination_phase, finite_model_found, szs_status

def bloodsuck(file_path, mode=["--mode", "casc_sat"], timeout=7,vampire_path=""):
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

def massacer(folder_path, mode=["--mode", "casc_sat"], timeout=7,vampire_path =""):
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
        return pd.DataFrame()

    # Iterate over all .p files in the folder
    for filename in sorted(os.listdir(folder_path)):
        if filename.endswith(".p"):
            file_path = os.path.join(folder_path, filename)
            result = bloodsuck(file_path, mode, timeout, vampire_path)
            results.append(result)

    # Convert results to a Pandas DataFrame
    results_df = pd.DataFrame(results)

    return results_df


# Modes
casc = ["--mode", "casc_sat"]
model_builder = ["-sa", "fmb"]
vampire_path ="../../vampire_build/vampire/bin/"

# Example usage
if __name__ == "__main__":
    folder = "generated_testfiles/"  # Update this with your actual folder path
    vampire_results_df = massacer(folder, mode=model_builder,vampire_path=vampire_path)

    print(vampire_results_df.head())
    vampire_results_df.to_csv("vampire_testsuite_results2.csv", index=False) 

#testsuite until scrupulousCurt (BB book) 
