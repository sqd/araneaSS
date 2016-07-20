# araneaSS
适合于远程操控的Shadowsocks实现<br>
A remote-controlled Shadowsocks implementation.
###[v0.1-amd64](https://github.com/sqd/araneaSS/releases/download/v0.1/v0.1-amd64.tar.gz)

---
可使用shell向远程服务器发送这些命令:<br>
Command available:

####`ping`<br>
服务器会回复`pong`<br>
Receive a `pong` from the server.

####`add password`<br>
添加一个密码为`password`, 加密方式为AES-256-CFB的SS服务端口, 服务器会返回端口号<br>
Start an AES-256-CFB encrypted SS server with password `password`, port number in server response.

####`remove port`<br>
删除在`port`上监听的SS服务, 服务器会回复`ok`<br>
Remove the SS server listening on port `port`. Server response is `ok`.

####`stats`<br>
返回各端口的流量数据, 格式为`show Data.Map.Map`<br>
Return the statistics of ports, in `show Data.Map.Map`'s format.

####`clear`<br>
清除统计数据, 服务器会回复`ok`<br>
Clear statistics. Server response is `ok`.

####`test server port password target`<br>
尝试通过位于`server:port`处, 密码为`password`, 加密方式为AES-256-CFB的SS服务器向`target`域名的80端口发出HTTP请求, 若成功则返回`True`, 失败则返回`False`, 网络错误则返回`error`<br>
Try send HTTP request to `target`'s port 80 via the SS server (if there is one) listening on `server:port`, with password `password` and encryption AES-256-CFB. Return `True` on success, `False` on failure, and `error` on network error.

