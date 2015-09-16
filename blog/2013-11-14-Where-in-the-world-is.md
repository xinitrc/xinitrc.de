---
title: Where in the world is ...
tags: openpaths, angularjs, locating, leaflet
bgimage: /assets/images/compass.jpg
position: tl
credit: gwen roolf
crediturl: https://www.flickr.com/photos/nimariel/
---

As you might have already seen on my [facts](/facts.html) page I have
my current location printed on a map and since being asked how I do
that I think it's better to share here then answer more emails. 


<!--more-->

## Openpaths 

First I need some service to get my current location, for this I
resorted to [Openpaths](http://openpaths.cc) they have an application
for iOS and Android that tracks your location and updates the position
on their site. At least part of the idea is to make those location
data available for science, which is probably why they have a simple
API to retreive them.  

Since I don't actually need anything fancy I can simply use the python
script openpath provides as an [API example](https://openpaths.cc/api)
with the slide modification that I only want the last point and use
<span class="tt">{'num_points':1}</span> as the parameter to the
request.


~~~ {.python}
#!/usr/bin/env python        

import oauth2, time, urllib, urllib2, json

ACCESS = "YOURACCESSKEY"
SECRET = "YOURSECRETKEY"
URL = "https://openpaths.cc/api/1" 
def build_auth_header(url, method):
    params = {
        'oauth_version': "1.0",
        'oauth_nonce': oauth2.generate_nonce(),
        'oauth_timestamp': int(time.time()),
    }
    consumer = oauth2.Consumer(key=ACCESS, secret=SECRET)
    params['oauth_consumer_key'] = consumer.key
    request = oauth2.Request(method=method, url=url, parameters=params)
    signature_method = oauth2.SignatureMethod_HMAC_SHA1()
    request.sign_request(signature_method, consumer, None)
    return request.to_header()

# GET data (last 24 hours)
now = time.time()
# params = {'start_time': now - 24*60*60, 'end_time': now}    # get the last 24 hours
params = {'num_points': 1 }
query = "%s?%s" % (URL, urllib.urlencode(params))
# print(query)
try:
    request = urllib2.Request(query)
    request.headers = build_auth_header(URL, 'GET')
    connection = urllib2.urlopen(request)
    data = json.loads(''.join(connection.readlines()))
    print(json.dumps(data, indent=4))
except urllib2.HTTPError as e:
    print(e.read())
~~~

This script is run every 5 minutes via a cronjob on my server and it's
output is piped to a file in the folder served by the webserver.


Since the file is already JSON, and provides <span
class="tt">lon</span> and <span class="tt">lat</span> information as
well as how long ago the last location change has been detected the
rest was simple. 

## The map 

For mapping I like [leaflet](http://leafletjs.com) especially since
there is already an
[AngularJS directive](https://github.com/tombatossals/angular-leaflet-directive). So
I simply have to load the necessary css and javascript files. 

~~~ {.html}
<link href="/scripts/leaflet-0.6.4/leaflet.css" rel="stylesheet" media="screen">
<script type="text/javascript" src="/scripts/leaflet-0.6.4/leaflet.js"></script>
<script type="text/javascript" src="/scripts/angular-leaflet-directive.js"></script>
~~~

And write a small Angular module. This is only the part of the script
I use for my whole site, but I made it self contained so it should work
by simply copying it and providing a file with the necessary data at
location in the call to <span class="tt">$http.get</span>.

~~~ {.javascript}
var app = angular.module('xinitrc', ["leaflet-directive"]);

var whereaboutCTRL = function ($scope, $http) {

    var calctime = function (t) {
        var now = new Date().getTime();

        var intervals = ['second', 'minute', 'hour'];
        var i = 0;
        var dist =  (now/ 1000) - t ;

        while (dist >= 60 && i < 2) {
            dist /= 60;
            i = i + 1;
        }

        var str = "Last seen here about " + Math.floor(dist) + " " + intervals[i];
        if (Math.floor(dist) !== 1) {
            str += "s"
        }
        str += " ago and mostly stationary since.";

        return str;
    }

    $http.get('/data/location.json').success(function (data) {
        if (data && data[0] && data[0].lon && data[0].lat && data[0].t) {
            $scope.center.lng = data[0].lon;
            $scope.center.lat = data[0].lat;
            $scope.markers.whereabouts.lng = data[0].lon;
            $scope.markers.whereabouts.lat = data[0].lat;

            $scope.message = calctime(data[0].t);
        }
    });

        angular.extend($scope, {
        center: {
            lng: 8.176630973815918,
            lat: 53.143672943115234,
            zoom: 16
        },
        markers: {
            whereabouts: {
                lat: 0, /*53.143672943115234,*/
                lng: 0, /*8.176630973815918,*/
                message: "Last seen here",
            }
        },
        defaults: {
            maxZoom: 16
        }
    });
}

whereaboutCTRL.$inject = ['$scope', '$http'];

app.controller('WhereaboutCTRL', whereaboutCTRL);
~~~

After getting the information via Ajax. I only need to provide them in
the <span class="tt">$scope</span> where they can be picked up by the
leaflet directive. Which looks as follows and is contained in the
<span class="tt">facts.html</span> page.

~~~ {.html}
<div>
	<div ng-controller="WhereaboutCTRL">
        <leaflet center="center" markers="markers" defaults="defaults"></leaflet>
		<div ng-bind="message"></div>
	</div>
</div>
~~~

## Conclusion

As with everything you copy from this site, do whatever you want with
it, but think about what you are doing. In the end you are providing
your location to the whole world all the time. At least this has the
potential to do so.
