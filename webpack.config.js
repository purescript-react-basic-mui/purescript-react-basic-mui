/* jshint esversion: 6 */
/* jshint node: true */
/* jshint -W097 */

'use strict';

const path = require('path');
const webpack = require('webpack');

module.exports = {
  mode: 'development',
  entry: './examples/app.js',
  output: {
    pathinfo: true,
    filename: 'bundle.js'
  },
  devServer: {
    contentBase: './examples/dist',
  },
};
