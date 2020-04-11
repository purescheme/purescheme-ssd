// const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = ({ mode }) => {
  return {
    mode, 
    entry: {
      vaadin: '@vaadin/vaadin',
    },
    devtool: mode === 'development' ? 'source-map' : 'none' 
    /*,
    optimization: {
      splitChunks: {
        chunks: 'all',
        minChunks: 1
      }
    },
    plugins: [
      new HtmlWebpackPlugin({
        template: './src/index.html' 
      })
    ] */
  };
};
