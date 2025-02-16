import os
import csv
import pandas as pd

def generate_tptp_files(csv_path, output_folder, separator=';', axioms_file="", logic="fof"):
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
        'info_pos_check': '{}(info_pos_check, axiom, (({}) => ({}))).\n',
        'info_neg_check': '{}(info_neg_check, axiom, ~(({}) => ({}))).\n',
        'cons_pos_check': '{}(cons_pos_check, axiom, ({} & {})).\n',
        'cons_neg_check': '{}(cons_neg_check, axiom, ({}) => ~({})).\n'
    }

    generated_files = 0
    processed_items = 0

    # Iterate over each row in the DataFrame
    for _, row in df.iterrows():
        processed_items += 1
        formula_id = row['id']
        p = row['p']
        q = row['q']

        # Generate and write TPTP files for each template
        for suffix, template in templates.items():
            tptp_content = ""
            # read in axioms_file
            if axioms_file:
                with open(axioms_file, 'r') as file:
                    axioms = file.read()
                tptp_content += f"{axioms}\n\n"

            tptp_content += f"% p = {q}\n% q = {p}\n"  # Add comments with p and q
            tptp_content += template.format(logic,q,p)
            filename = f"{formula_id}_{suffix}.p"
            file_path = os.path.join(output_folder, filename)
            with open(file_path, mode='w') as file:
                file.write(tptp_content)
            generated_files += 1

    print(f"Generated {generated_files} TPTP files for {processed_items} item(s) in '{output_folder}'.")


import argparse

def main():
    parser = argparse.ArgumentParser(description="Generate TPTP test files from a CSV file.")
    parser.add_argument("csv_file", type=str, help="Path to the CSV file containing test cases.")
    parser.add_argument("output_dir", type=str, help="Directory where generated test files will be stored.")
    parser.add_argument("--axioms_file", type=str, default=None, help="Path to the axioms file (optional).")
    parser.add_argument("--logic", type=str, default="tff", help="Logic type (default: tff).")

    args = parser.parse_args()

    generate_tptp_files(args.csv_file, args.output_dir, axioms_file=args.axioms_file, logic=args.logic)

if __name__ == "__main__":
    main()