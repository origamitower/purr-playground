const path = require("path");

module.exports = {
  entry: path.join(__dirname, "Data.fsproj"),
  outDir: path.join(__dirname, "../build/package"),
  babel: {
    presets: [["@babel/env", { modules: "commonjs" }]],
    sourceMaps: true
  }
};