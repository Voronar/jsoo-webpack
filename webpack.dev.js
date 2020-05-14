const path = require('path');

module.exports = {
  entry: ['./app.js', './_build/default/main.bc.js'],
  mode: 'development',
  // devtool: false,
  target: 'web', 
  // mode: 'production',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, '_build', 'default'),
  },
};
