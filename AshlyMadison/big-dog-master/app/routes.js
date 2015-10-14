var Nerd = require('./models/nerd');

module.exports = function(app) {
	// server routes
	app.get('/api/nerds', function(req, res) {
		// mongoose
		Nerd.find(function(err, nerds) {
			// error retrieving 
			if (err) {
				res.send(err);
			}

			// nerds in json format
			res.json(nerds);
		});
	});

	// app.post routes
	// app.delete routes

	app.get('*', function(req,res) {
		// route to angular frontend
		res.sendfile('./public/views/index.html');
	});
};