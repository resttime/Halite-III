const webpack = require("webpack");
const path = require("path");
const VueLoaderPlugin = require('vue-loader/lib/plugin');
const MomentLocalesPlugin = require('moment-locales-webpack-plugin');

// https://github.com/vuejs-templates/webpack-simple
module.exports = {
    entry: {
        main: ['babel-polyfill', './javascript/main.js'],
        blockly: ['babel-polyfill', './learn-programming-challenge/blockly/glitch.js'],
    },
    output: {
        publicPath: "/assets/js/",
        path: path.resolve(__dirname, "assets/js/"),
        filename: "bundle-[name].js",
    },
    module: {
        noParse: /libzstd/,
        rules: [
            {
                test: /\.js$/,
                exclude: /(node_modules|libzstd)/,
                use: {
                    loader: 'babel-loader',
                }
            },
            {
                test: /\.scss$/,
                use: [
                    'vue-style-loader',
                    'css-loader',
                    'sass-loader'
                ]
            },
            {
                test: /\.vue$/,
                loader: 'vue-loader',
            },
            {
                test: /\.css$/,
                loader: 'style-loader!css-loader'
            },
            {
                test: /\.(png|ttf|woff)$/,
                loader: "file-loader",
            },
        ],
    },
    plugins: [
        new VueLoaderPlugin(),
        new webpack.DefinePlugin({
            'process.env.ASSET_PATH': JSON.stringify("/assets/js/")
        }),
        new MomentLocalesPlugin({
            localesToKeep: ['es-us', 'ru'],
        })
    ],
};
