var net = require('net');
var cluster = require('cluster');
var numCPUs = 4;

if (cluster.isMaster) {
    for (var i = 0; i < numCPUs; i++) {
        cluster.fork();
    }
} else {
    var server = net.createServer(function(socket) {
        socket.on('data', function(data){
        socket.write(respond)
      })
    });
    server.listen(8888);
}

var respond =

"HTTP/1.1 200 OK\r\n\
Content-Type: text/html; charset=UTF-8\r\n\
Content-Length: 500\r\n\
Connection: Keep-Alive\r\n\r\n\
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"



