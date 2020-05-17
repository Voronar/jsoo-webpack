const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: ['./app.js', './_build/default/main.bc.js'],
  mode: 'development',
  // mode: 'production',
  // devtool: false,
  target: 'web', 
  output: {
    publicPath: '/',
    filename: 'app.js',
    path: path.resolve(__dirname, '_build', 'default'),
  },
  module: {
    rules: [
      {
        test: /\.less$/,
        use: [
          {
            loader: 'style-loader',
          },
          {
            loader: 'css-loader',
          },
          {
            loader: 'less-loader',
            options: {
              lessOptions: {
                javascriptEnabled: true,
              },
            },
          },
        ],
      },
    ],
  },
  devServer: {
    contentBase: path.resolve(__dirname, '_build', 'default'),
    // compress: true,
    port: 9000
  },
  plugins: [
    new webpack.HotModuleReplacementPlugin({}),
    new HtmlWebpackPlugin({
      title: 'Development',
      template: path.resolve(__dirname, 'index.html'),
      filename: 'index.html',
    }),
  ],
};
