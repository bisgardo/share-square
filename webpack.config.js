const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin')

// Mode defaults to "production" if not specified using the CLI flag '--mode'.

module.exports = (env, argv) => ({
    entry: './src/webpack-entrypoint.js',
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'app.js',
    },
    plugins: [
        // Generate 'index.html'.
        new HtmlWebpackPlugin({
            title: "Share 'n Square | Initializing...",
        }),
    ],
    module: {
        rules: [
            {
                test: /\.css$/,
                use: [
                    'style-loader',
                    'css-loader',
                ]
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: [
                    {
                        loader: 'elm-hot-webpack-loader'
                    },
                    {
                        loader: 'elm-webpack-loader',
                        options: {
                            cwd: __dirname,
                            // debug is enabled by default in "development" mode
                            // optimize is enabled by default in "production" mode
                        },
                    },
                ],
            },
        ],
    },
    devServer: {
        inline: true,
        hot: true,
        stats: 'errors-only',
    },
});
