// create an array with nodes
$.getJSON ("/nodes", function (data) {
    var nodes = new vis.DataSet(data);
    // create an array with edges
    $.getJSON ("/edges", function (data) {
	var edges = new vis.DataSet(data);
	
	// create a network
	var container = document.getElementById('mynetwork');
	var data = {
	    nodes: nodes,
	    edges: edges
	};
	var options = {
	    autoResize: true,
	    height: '100%',
	    width: '100%'
	};
	var network = new vis.Network(container, data, options);
	//$("#mynetwork > canvas").css("height", "1000px");
    });
});
