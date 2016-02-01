# test for google directions api request

# url https://maps.googleapis.com/maps/api/directions/json?origin=48.8780829,2.2806976&destination=48.8782716,2.4084414

import requests

conf = open('conf/key','r')
key = conf.readline().replace('\n','')
#print(key)
result = requests.get('https://maps.googleapis.com/maps/api/directions/json?origin=48.8780829,2.2806976&destination=48.8782716,2.4084414&departure_time=now&traffic_model=best_guess&key='+key)
data = result.json()

#print(data)

print(data['routes'][0]['legs'][0]['duration_in_traffic']['value'])
