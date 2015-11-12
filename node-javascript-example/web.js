var express = require("express");
var bodyParser = require("body-parser");
var app = express();

var request = require("request");



/* var mongoUri = process.env.MONGOLAB_URI || proces.env.MONGOHQ_URL ||
                  'mongodb://localhost:27017/pickitup';


var mongo = require('mongodb');

var MongoClient = mongo.MongoClient;


var db = mongo.Db.connect(mongoUri, function(error, databaseConnection) {
    db = databaseConnection;
   
   }); */




app.all('*', function(req,res,next) {
     res.header("Access-Control-Allow-Origin", "app/views");
     res.header("Access-Control-Allow-Headers", "X-Requested-With");
     res.header("Access-Control-Allow-Headers", "PUT, GET, POST, DELETE, OPTIONS");
     next();

     });


app.use(express.static('app'));
app.use(bodyParser.urlencoded({extended:true }));
app.set('views', (__dirname, 'app/views'));
app.engine('html', require('ejs').renderFile);

app.set('view engine', 'ejs');


app.get('/', function(req, res) {
         res.render('index.html');
});

app.get('/tripadvisor', function(req, res) {
   pinLat = req.query.pinLat;
   pinLng = req.query.pinLng;

  toSend = "https://api.tripadvisor.com/api/partner/2.0/map/" + pinLat + ',' +
            pinLng + "/restaurants?key=CC3B76F2F0BE44469D6610344CC8E104";
console.log(toSend);
  request(toSend, function(error, response, body) {
	console.log(body);
	if (!error && response.statusCode == 200) {
		res.send(body);
	}
   });
           
});

app.post("/fromTo", function(req, res) {

  res.render("withMap.html");

 });








var port = Number(process.env.PORT || 5000);



app.listen(port, function() {

  var t = 1;

 });
