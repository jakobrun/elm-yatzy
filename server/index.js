'use strict';

const express = require('express'),
  port = process.env.PORT || 3030,
  app = express();

app.use(express.static('build'));
app.use(express.static('web'));

app.listen(port, function() {
  console.log('server listening on port:', port);
});
