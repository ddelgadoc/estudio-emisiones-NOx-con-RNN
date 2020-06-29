# -*- coding: utf-8 -*-
"""
Created on Mon Apr  6 18:04:54 2020

@author: deyli
"""

import pandas as pd
import glob

##Concatenar hojas de cada excel en un solo fichero
def conc(archivo,name):
    xls=pd.ExcelFile(archivo,header=14,skiprows=13)  #archivo= 'PlenaCarga_B100.xlsx'
    hojas=[]
    for h in xls.sheet_names:
        hojas.append(xls.parse(h))
    final=pd.concat(hojas,ignore_index=True)
    R=pd.ExcelWriter(name)    #name='Final_Plena.xlsx'
    final.to_excel(R,'Hoja1')
    R.save()

#print(final.head(10))
Archn=[('PlenaCarga_B100.xlsx','Final_Plena.xlsx'),('MediaCarga_B100.xlsx','Final_Media.xlsx'),('Vacio_B100.xlsx','Final_Vacio.xlsx')]
 
for i in range(0,3):
    Result=conc(Archn[i][0],Archn[i][1])
    Result

##Agregar las variables Acel, Sobreacel y Carga a los ficheros(después de modificaciones en Excel para eliminar filas intermedias y crear nombres de las columnas)
def agregar(archivo,name,No): 
    df=pd.read_excel(archivo)
    df['Carga']=No   #agregar variable estado de la carga 0= vacio,1=media carga,2=carga plena
    df['Aceleracion']=(df.Velocity - df.Velocity.shift())/3.6 #agregar aceleracion en m/s2, 3.6 conversión a m/s2
    df['Sobreace']=df.Aceleracion - df.Aceleracion.shift()  #agregar sobreaceleracion en m/s3
    R=pd.ExcelWriter(name)
    df.to_excel(R,'Hoja1')
    R.save()
M=[('PlenaCarga_VarAgregadas.xlsx','Final_Plena.xlsx'),('MediaCarga_VarAgregadas.xlsx','Final_Media.xlsx'),('Vacio_VarAgregadas.xlsx','Final_Vacio.xlsx')]
Num=[2,1,0]
for i in range(0,3):
    Result=agregar(Archn[i][1],Archn[i][0],Num[i])
    Result

##Unir todos los fichero de cada estado de carga en un solo fichero,antes dividir los ficheros en train y test
todos=[]
for f in glob.glob('*.xlsx'):  #solo dejar en la carpeta donde está el código los ficheros que quiero unir
    df=pd.read_excel(f)
    todos.append(pd.read_excel(f))
    
df=pd.concat(todos,ignore_index=True)

R=pd.ExcelWriter('Final_todos_train.xlsx')
df.to_excel(R,'Hoja1')
R.save()

print(df.shape) #Ver cantidad de filas y columnas, antes de la limpieza 

##Limpiar el fichero(negativos sustituir mediana y N.A eliminar)
df=pd.read_excel('Final_todos_train.xlsx')

#df=pd.read_csv('Train_estandarizado.csv')
#Contar valores negativos de las emisiones de NOx
R=[]
for index, row in df.iterrows():
    if row['NOx mass']<0:
        R.append(row['NOx mass'])
        
print('Cantidad=' + str(len(R)))
#Sustituir valores negativos por la mediana
for index, row in df.iterrows():
    if row['NOx mass']<0:
        df['NOx mass'][index] = 0.0519828 #mediana de la variable NOx

R=pd.ExcelWriter('Final_Train_Sin_valores_erroneos.xlsx')
df.to_excel(R,'Hoja1')
R.save()

print(df.isnull().sum()) #calcular cantidad de valores NAN o nulos por culumna, no hay N.A

print(df.shape) #cantidad de filas y columnas después de la limpieza