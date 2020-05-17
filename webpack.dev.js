const path = require('path');

module.exports = {
  entry: ['./app.js', './_build/default/main.bc.js'],
  mode: 'development',
  // mode: 'production',
  // devtool: false,
  target: 'web', 
  output: {
    filename: 'app.js',
    path: path.resolve(__dirname, '_build', 'default'),
  },
};
