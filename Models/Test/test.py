# test for google directions api request

# url https://maps.googleapis.com/maps/api/directions/json?origin=48.8780829,2.2806976&destination=48.8782716,2.4084414

import requests,numpy,time,sqlite3

conf = open('conf/key','r')
key = conf.readline().replace('\n','')
#print(key)

# get random link with coords from coords file
coordsfile = open('conf/linkcoords.csv','r')
coords = coordsfile.readlines()
coordline = coords[numpy.random.choice(len(coords),1)].replace('\n','').split(';')
linkid=coordline[0]
origin=coordline[2]+','+coordline[1]
destination=coordline[4]+','+coordline[3]

result = requests.get('https://maps.googleapis.com/maps/api/directions/json?origin='+origin+'&destination='+destination+'&departure_time=now&traffic_model=best_guess&key='+key)
data = result.json()

#print(data)
# print(data['routes'][0]['legs'][0]['duration_in_traffic']['value'])
traveltime=data['routes'][0]['legs'][0]['duration_in_traffic']['value']

conn = sqlite3.connect('data/gmaps.sqlite3')
c = conn.cursor()
t = time.time()
c.execute('INSERT INTO data VALUES ('+str(t)+','+str(linkid)+','+str(traveltime)+');')
conn.commit()
conn.close()
