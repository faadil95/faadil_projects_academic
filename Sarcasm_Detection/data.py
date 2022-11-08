from kaggle.api.kaggle_api_extended import KaggleApi
from zipfile import ZipFile
import pandas as pd
import string

api = KaggleApi()
api.authenticate()

# Download Dataset
api.dataset_download_files('rmisra/news-headlines-dataset-for-sarcasm-detection')

# Unzip Dataset
zf = ZipFile('news-headlines-dataset-for-sarcasm-detection.zip')
zf.extractall('data\\') 
zf.close()

#Read Dataset and Print
df = pd.read_json("data/Sarcasm_Headlines_Dataset.json", lines=True)
df.to_csv('sarcasm.csv')

print(df.head(5))