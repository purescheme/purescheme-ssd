
module.exports = ({ mode }) => {
  return {
    mode, 
    entry: {
      'ssd-driver': './src/ssd-driver.js'
    },
    devtool: mode === 'development' ? 'source-map' : 'none' 
  };
};
