const webpack = require("webpack");
const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const ExtractTextPlugin = require("extract-text-webpack-plugin");

const src = path.resolve(__dirname);
const dist = path.resolve(__dirname, "./dist/");

const config = {
  devtool: "#source-map",
  entry: {
    app: path.resolve(src, "main.js"),
    vendor: ["react", "react-dom"]
  },
  output: {
    path: dist,
    filename: "[name].bundle.js"
  },
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader",
          options: {
            presets: ["@babel/preset-env", "@babel/preset-react"]
          }
        }
      },
      {
        test: /\.html$/,
        use: "html-loader"
      },
      {
        test: /\.css$/,
        use: ExtractTextPlugin.extract({
          fallback: "style-loader",
          use: [
            {
              loader: "css-loader"
            },
            {
              loader: "sass-loader"
            },
            {
              loader: "postcss-loader"
            }
          ]
        })
      }
    ]
  },
  resolve: {
    extensions: [".js", ".jsx"]
  },
  plugins: [
    new HtmlWebpackPlugin(),
    new ExtractTextPlugin("assets/css/application.css")
  ]
};

module.exports = config;
