import sys
from importlib.metadata import distributions
from src.dataLoader import load_csv_dataset
from src.utils import select_csv_file
from src.translate import translate_equipment
import os
import glob


def main():
    """
    Main function to print Python version and installed libraries.
    """
    print("Python Environment Information")
    print("-------------------------------")
    print(f"Python version: {sys.version}\n")
    print("Installed libraries:")
    for dist in sorted(distributions(), key=lambda d: d.metadata['Name'].lower()):
        name = dist.metadata['Name']
        version = dist.version
        print(f"{name}=={version}")

if __name__ == "__main__":

    main()
   
    folder = input("Enter the path to the data folder: ").strip()
    if not os.path.isdir(folder):
        print("Invalid folder path.")
        sys.exit(1)

    csv_files = [os.path.basename(f) for f in glob.glob(os.path.join(folder, "*.csv"))]
    if not csv_files:
        print("No CSV files found in the folder.")
        sys.exit(1)

    selected_file = select_csv_file(csv_files)
    csv_path = os.path.join(folder, selected_file)
    
    # Realizar o translate das metricas e ter um DataFrame com uma linha por equipamento
    
    data = translate_equipment(csv_path)
    print("\nLoaded data from CSV:")
    print(data.head()) 
    