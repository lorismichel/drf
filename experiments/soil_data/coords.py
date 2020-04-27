import pandas as pd 
# Read data from file 'filename.csv' 
# (in the same directory that your python process is based)
# Control delimiters, rows, column names with read_csv (see later) 
data = pd.read_csv("~/Downloads/data_for_ETH_2.csv") 
# Preview the first 5 lines of the loaded data 


data.head()


from pyproj import Proj, transform

inProj = Proj(init='epsg:2154')
outProj = Proj(init='epsg:4326')

#ifor index, row in data.iterrows():
#  x1,y1 = row["X"], row["Y"]
#  x2,y2 = transform(inProj,outProj,x1,y1)
#  print(x2,y2)
#for p in data:
#  print(p)

data_out = pd.DataFrame(
    transform(inProj,outProj,data["X"][p],data["Y"][p]) for p in range(data.shape[0])
)

print(data_out.head())

data_out.to_csv('~/Downloads/coords_soil_2.csv',encoding='utf-8', index = False, header=False)
