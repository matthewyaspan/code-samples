var express = require('express');
var app = express();
var request = require('request');

// Mongo Initialization, setting up a connecton to a MongoDB (on Heroku or localhost)

var mongoUri = process.env.MONGOLAB_URI || process.env.MONGOHQ_URL || 
	       'mongodb://localhost:27017/whereintheworld';

var mongo = require('mongodb');

var MongoClient = mongo.MongoClient;


app.all('/', function(req, res, next) {
	res.header("Access-Control-Allow-Origin", "*");
	res.header("Access-Control-Allow-Headers", "X-Requested-With");
	res.header("Access-Control-Allow-Methods", "PUT, GET, POST, DELETE, OPTIONS");
	next();
});



app.use(express.static('app'));
app.set('views', (__dirname, 'app/views'));
app.engine('html', require('ejs').renderFile);
app.set('view engine', 'ejs');


app.get('/', function (req, res) {
	var htmlhead = '<!DOCTYPE html><html><head><title>Locations</title></head>' + 
		   '<body><h1>Locations</h1>';
	var htmlbodyend = '</body></html>';

	var mid = '';

	MongoClient.connect(mongoUri, function (err, db) {

	  var collection = db.collection('locations');

	  
	  var array = collection.find().sort({_id:-1}).toArray(function (err, results) {
	
	    results.forEach(function(r) {
		mid = mid +  '<p> Time: ' + JSON.stringify(r.created_at)
		          + ' name: ' +  JSON.stringify(r.login)
			  + ' lat: ' + JSON.stringify(r.lat)
			  + ' lng: ' + JSON.stringify(r.lng) +  '</p><br/>';
	    });
	
	   res.send(htmlhead + mid + htmlbodyend);

	   db.close();
	  });

       });

});



var bodyParser = require('body-parser');
app.use(bodyParser.urlencoded({
	extended: true
}));



app.post('/sendLocation', function (req, res) {


  if (req.body.login && req.body.lat && req.body.lng) {

    res.header("Access-Control-Allow-Origin", "*");
    res.header("Access-Control-Allow-Headers", "X-Requested-With");
    res.header("Access-Control-Allow-Methods", "PUT, GET, POST, DELETE, OPTIONS");

    MongoClient.connect(mongoUri, function (err, db) {

     var date = new Date();
     date = date.toUTCString();

     var collection = db.collection('locations', function (err, collection) {
      if (err) sys.puts(err);
      else {

	collection.insert({"login": req.body.login,
                           "lng": req.body.lng,
                           "lat": req.body.lat,
                           "created_at": date},
			   function(err, records) {
				
	  if (err) sys.puts(err); 
          else {

	    collection.find().sort({_id:-1}).limit(100).toArray(function (err,
									results){
		
		temp = JSON.stringify(results);
		res.send('{ "characters": [], "students":'
		+ temp
		+ '}');

		db.close();
		});
	    }});
	}});
	     
	});
	}  else {
		res.send("failed, insufficient parameters");

	}
});


app.get('/locations.json', function(req, res) {
	if (req.query.login) {

          MongoClient.connect(mongoUri, function (err, db) {

	    var collection = db.collection('locations');
	    
 	    collection.find({login : req.query.login}).toArray( function(err, results){
			res.json(results);
		});
	  });
	
	} else {
		res.send('[]');

	}
	
});

	
app.get('/redline.json', function (req, res) {
	var url = "http://developer.mbta.com/lib/rthr/red.json";

	request({url: url, json: true }, function (error, response, body) {
		
		res.send(response);
	});
		
});

var port = Number(process.env.PORT || 5000);

app.listen(port, function() {
	var t = 1;
});
