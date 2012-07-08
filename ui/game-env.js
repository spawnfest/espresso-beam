// Authors: Loris Fichera, Mirko Bonadei, Paolo D'Incau
// Created: 7 July 2012

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

    update_canvas(msg);
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
}  

function stepFunction() {
    interval = setInterval(function() { doSend("step") }, 1000);
}

function stopFunction() {
    clearInterval(interval);
    doSend("stop");
}

window.addEventListener("load", init, false);  

// here are the functions for drawing

//images
var imgRabbit = "img/rabbit.png"
var imgCarrot = "img/carrot.png"
var imgWolf   = "img/tac-nayn.png"

//tile size
var tileSize = 32;

// canvas and drawing context
var cvs;
var ctx;

// canvas size
var cWidth  = 30;
var cHeight = 18;
  
// convert cell coordinates into canvas coordinates, according to the tile size
function cell2CanvasCoord(coord) {
    return [coord[0] * tileSize, coord[1] * tileSize];
}

// draw a character on the canvas
function draw(character, x, y) {    
    var img = new Image();
    
    if (character == 'rabbit')
	img.src = imgRabbit;
    
    else if (character == 'wolf') 
	img.src = imgWolf;
    
    else if (character == 'carrot') 
	img.src = imgCarrot;
    
    canvasCoord = cell2CanvasCoord([x, y]);
    ctx.drawImage(img, canvasCoord[0], canvasCoord[1]);
}

// update the canvas
function update_canvas(msg) {
    // clear the canvas, first
    cvs.width  = cWidth * tileSize;
    
    var rabbitsTmp = 0;
    var carrotsTmp = 0;
    var wolvesTmp = 0;
    
    for (i=0; i < msg.env_state.length; i++) {
        type = msg.env_state[i].type;

        if (type == 'rabbit')
            rabbitsTmp++;
        else if (type == 'wolf')
            wolvesTmp++;
        else if (type == 'carrot')
            carrotsTmp++;

	    draw (msg.env_state[i].type, 
	        msg.env_state[i].location[0], 
	        msg.env_state[i].location[1]);
    }

    // updating data series
    rabbits[timeSteps] = new Array(timeSteps, rabbitsTmp);
    wolves[timeSteps] = new Array(timeSteps, wolvesTmp);
    carrots[timeSteps] = new Array(timeSteps, carrotsTmp);
    timeSteps++;

    $.plot(
        $("#placeholder"),
        [
            {
                label: "Rabbits",
                color: "green",
                data: rabbits
            },
            {
                label: "Carrots",
                color: "orange",
                data: carrots
            },
            {
                label: "Wolves",
                color: "black",
                data: wolves
            }
        ],
        {
            series: {
                lines: { show: true },
                points: { show: true}
            },
            grid: {
                backgroundColor: { colors: ["#fff", "#eee"] }
            }
        }
    );
}

window.onload = function () {
    // get the image context
    cvs = document.getElementById('game-canvas');
    ctx = cvs.getContext('2d');
    
    //resize the canvas
    cvs.width  = cWidth * tileSize;
    cvs.height = cHeight * tileSize;
}

// Here are the functions for plotting the graphs

var rabbits = []; 
var carrots = [];
var wolves = [];
var timeSteps = 0;
