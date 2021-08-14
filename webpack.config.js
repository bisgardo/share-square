const path = require('path');
const webpack = require('webpack');

// Mode defaults to "production" if not specified using the CLI flag '--mode'.

module.exports = (env, argv) => ({
    entry: './src/webpack-entrypoint.js',
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'app.js',
    },
    module: {
        rules: [
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: [
                    {loader: 'elm-hot-webpack-loader'},
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
    plugins: [
        new webpack.HotModuleReplacementPlugin(),
    ],
    devServer: {
        inline: true,
        hot: true,
        stats: 'errors-only',
    },
});
