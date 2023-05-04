const express = require('express');
const bodyparser = require('body-parser')
const cors = require('cors');
var app = express();
var port = process.env.PORT || '3001';
app.set('port', port)

//app.use(bodyparser.json())
app.use(bodyparser.json({ limit: '50mb', extended: true }));
app.use(bodyparser.urlencoded({ limit: '50mb', extended: true }));
//app.use(bodyparser.urlencoded({ extended: false }))
app.use(cors({ origin: 'http://localhost:4200' }))

app.post('/interpreter', (req, res) => {
    const parser = require('./grammar');
    try {
        var count = parser.parse(req.body.code);
        res.send({ output: count.getOutput(), errors: count.getErrorsTable(), symbols: count.getSymbolsTable() });
    } catch (e) {
        console.log(e)
        res.send({ output: '#*\n' + e + '\n*#',errors: [], symbols: [] });
    }

})

app.listen(app.get('port'), () => {
    console.log(`on port ${app.get('port')}`)
})