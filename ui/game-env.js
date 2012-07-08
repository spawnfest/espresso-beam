var wsUri = "ws://localhost:8080/websocket";
var output;  
var interval;

function init() { 
    output = document.getElementById("output"); 
    testWebSocket();  
}  

function testWebSocket() { 
    websocket = new WebSocket(wsUri); 
    websocket.onopen = function(evt) { onOpen(evt) }; 
    websocket.onclose = function(evt) { onClose(evt) }; 
    websocket.onmessage = function(evt) { onMessage(evt) }; 
    websocket.onerror = function(evt) { onError(evt) }; 
}  

function onOpen(evt) { 
    writeToScreen("Connected."); 
}  

function onClose(evt) {     
    clearInterval(interval);
    writeToScreen(evt); writeToScreen("Disconnected."); 
}  

function onMessage(evt) { 
    var msg = JSON.parse(evt.data);

    for (i=0; i < msg.env_state.length; i++) {
	writeToScreen('<span style="color: blue;">Reply: ' + msg.env_state[i].type +' </span>');
    }
}  

function onError(evt) { 
    writeToScreen('<span style="color: red;">Error:> ' + evt.data + '</span>'); 
}  

function doSend(message) { 
    writeToScreen("Sent: " + message);  
    websocket.send(message); 
}  

function writeToScreen(message) { 
    var pre = document.createElement("p"); 
    pre.style.wordWrap = "break-word"; 
    pre.innerHTML = message; 
    output.appendChild(pre); 
}  

function stepFunction() {
    interval = setInterval(function() { doSend("step") }, 1000);
}

function stopFunction() {
    clearInterval(interval);
    doSend("stop");
}

window.addEventListener("load", init, false);  
  