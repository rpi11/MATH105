import csv

years=(range(1960,2022))
categoriesAbbv=[]
categories=[]
countries=[]
indices=[]
intrCountries=[]
intrCateg=[]

with open("emission_categ.csv", "r") as em, open("QOL_categ.csv", "r") as qol:
    em_reader=csv.reader(em)
    qol_reader=csv.reader(qol)
    next(em_reader)
    next(qol_reader)
    for cat in em_reader:
        intrCateg.append(cat[0])
    for cat in qol_reader:
        intrCateg.append(cat[0])

with open("countries.csv", "r") as f:
    reader=csv.reader(f)
    for line in reader:
        intrCountries.append(line[0])

with open("WDIData.csv", "r") as f:
   reader=csv.reader(f)
   next(reader)
   j=0
   for line in reader:
        print(j)
        j+=1

        country=line[0]
        category=line[2]
        if category in intrCateg and country in intrCountries:

            
            if country not in countries:
                countries.append(country)
            if category not in categories:
                categoriesAbbv.append(line[3])
                categories.append(category)
            i=4
            while i < 66:
                indices.append(line[i])
                i+=1

with open("WDIData_FORMAT2.csv","w") as out:
    header=["Country", "Year", "Category", "Index"]
    writer=csv.writer(out, delimiter="|")
    writer.writerow(header)
    i=0
    for country in countries:
        for categ in categories:
            for year in years:
                if indices[i]!="":
                    output=[country, year, categ, indices[i]]
                    writer.writerow(output)
                print(i)
                i += 1








    