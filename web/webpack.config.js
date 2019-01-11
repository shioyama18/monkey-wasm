const CopyWebpackPlugin = require("copy-webpack-plugin");
const HtmlWebpackPlugin = require('html-webpack-plugin');
const path = require('path');

const SRC_PATH = path.resolve(__dirname, 'src');
const DIST_PATH = path.resolve(__dirname, 'dist');

module.exports = {
  entry: path.resolve(__dirname, 'src/js/bootstrap.js'),	
  output: {
    path: DIST_PATH,
    filename: 'bundle.[hash].js',
  },
  mode: "production",
  plugins: [
    new HtmlWebpackPlugin({
      template: path.join(SRC_PATH, 'index.html'),
      minify: true,
    }),
    new CopyWebpackPlugin([
      { from: '**/*', to: DIST_PATH, ignore: ['*.js', '*.css', '*.html'] },
    ], {
      context: 'src',
    }),
  ],
};
