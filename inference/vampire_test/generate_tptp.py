import os
import csv
import pandas as pd

def generate_tptp_files(csv_path, output_folder, separator=';'):
    """
    Generates TPTP files from a CSV file containing formulas p and q, including comments with the original formulas.

    Parameters:
    - csv_path (str): Path to the input CSV file with columns 'id', 'p', and 'q'.
    - output_folder (str): Directory where the TPTP files will be saved.
    - separator (str): The delimiter used in the CSV file (default is ';').
    """
    # Ensure the output directory exists
    os.makedirs(output_folder, exist_ok=True)

    # Read the CSV file using pandas
    df = pd.read_csv(csv_path, sep=separator)

    # Define TPTP templates with placeholders for p and q
    templates = {
        'pos_info_check': 'fof(single_formula, axiom, (({}) => ({}))).\n',
        'neg_info_check': 'fof(single_formula, axiom, ~(({}) => ({}))).\n',
        'pos_cons_check': 'fof(single_formula, axiom, ({} & {})).\n',
        'neg_cons_check': 'fof(single_formula, axiom, ({}) => ~({})).\n'
    }

    # Iterate over each row in the DataFrame
    for _, row in df.iterrows():
        formula_id = row['id']
        p = row['p']
        q = row['q']

        # Generate and write TPTP files for each template
        for suffix, template in templates.items():
            tptp_content = f"% p = {q}\n% q = {p}\n"  # Add comments with p and q
            tptp_content += template.format(q, p)
            filename = f"{formula_id}_{suffix}.p"
            file_path = os.path.join(output_folder, filename)
            with open(file_path, mode='w') as file:
                file.write(tptp_content)
            print(f"Generated {file_path}")

# Example usage:
generate_tptp_files('tptp_testsuite.csv', 'generated_testfiles')

