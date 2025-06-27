import pandas as pd

class Dataset:
    def __init__(self, data: pd.DataFrame):
        self.data = data

    def __len__(self):
        return len(self.data)

    def __getitem__(self, idx):
        return self.data.iloc[idx]

def load_csv_dataset(filepath: str) -> Dataset:
    df = pd.read_csv(filepath)
    return Dataset(df)