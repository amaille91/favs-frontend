const { createProxyMiddleware } = require("http-proxy-middleware");
const express = require('express');
const app = express();

app.use('/', createProxyMiddleware({
  target: 'http://localhost:8081',
  pathRewrite: {'^/api': ''}
}));
app.use(express.static('static'));

app.listen(1234, () => {
    console.log('port running on port number : 1234');
})
