'use strict';

const express = require('express'),
  path = require('path'),
  port = process.env.PORT || 3030,
  app = express();

app.use(express.static('build'));
app.use(express.static('web'));
const basePath = path.join(__dirname, '../node_modules/');
app.use('/basscss', express.static(path.join(basePath, 'basscss/css')));

app.listen(port, function() {
  console.log('server listening on port:', port);
});
