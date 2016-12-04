HTMLWidgets.widget({

  name: "forceNetwork4",

  type: "output",

  initialize: function(el, width, height) {

    d3.select(el).append("svg")
        .attr("width", width)
        .attr("height", height);

      return d3.forceSimulation();
  },

  resize: function(el, width, height, simulation) {
      d3.select(el).select("svg")
          .attr("width", width)
          .attr("height", height);
      simulation.force("center").x(width/2); // recenter the simulation
      simulation.force("center").y(height/2);
      simulation.alpha(1.0).restart(); // add some heat and restart it
  },

  renderValue: function(el, x, simulation) {
      // alias options
      var options = x.options;
      
      // convert links and nodes data frames to d3 friendly format
      var links = HTMLWidgets.dataframeToD3(x.links);
      var nodes = HTMLWidgets.dataframeToD3(x.nodes);

      // get the width and height
      var width = el.offsetWidth;
      var height = el.offsetHeight;

      var node_radius_function = Function("d",options.node_radius);
      var node_fill_colour_function = Function("d",options.node_fill_colour);
      var node_fill_opacity_function = Function("d",options.node_fill_opacity);
      var node_stroke_colour_function = Function("d",options.node_stroke_colour);
      var node_stroke_width_function = Function("d",options.node_stroke_width);
      var node_label_opacity_function = Function("d",options.node_label_opacity);
      var link_stroke_width_function = Function("d",options.link_stroke_width);
      var link_stroke_colour_function = Function("d",options.link_stroke_colour);
      var link_curvature_function = Function("d",options.link_curvature);
      var link_distance_function = Function("d",options.link_distance);
      var link_onclick_function = Function("d",options.link_onclick);
      var link_mouseover_function = Function("d",options.link_mouseover);
      var link_mouseout_function = Function("d",options.link_mouseout);
      var node_mouseover_function;
      if (options.node_mouseover=="default") {
	  node_mouseover_function=default_node_mouseover;
      } else {
	  node_mouseover_function = Function("d",options.node_mouseover);
      }
      if (options.node_mouseout=="default") {
	  node_mouseout_function=default_node_mouseout;
      } else {
	  node_mouseout_function = Function("d",options.node_mouseout);
      }

    // select the svg element and remove existing children
    var svg = d3.select(el).select("svg");
      svg.selectAll("*").remove();

      // arrow heads
      svg.append("svg:defs").selectAll("marker")
	  .data(["end"])      // Different link/path types can be defined here
	  .enter().append("svg:marker")    // This section adds in the arrows
	  .attr("id", String)
	  .attr("viewBox", "0 -5 10 10")
	  .attr("refX",5)
	  .attr("refY",0)// -1.5)
	  .attr("markerWidth", 2)
	  //.attr("markerHeight",4)
	  .attr("orient", "auto")
	  .append("svg:path")
	  .attr("d", "M0,-5L10,0L0,5");

      // first put down an invisible rect which will catch the zoom and pan
      //  actions
      if (options.zoom) {
	  svg.append("rect")
	      .attr("width", width)
	      .attr("height", height)
	      .style("fill", "none")
	      .style("pointer-events", "all")
	      .call(d3.zoom()
		    .scaleExtent([0.5,4])
		    .on("zoom", zoomed));
      }
      
      // draw links as straight lines
/*      var link = svg.append("g")
	  .attr("class", "links")
	  .selectAll("line")
	  .data(links)
	  .enter().append("line")
	  .style("stroke",function(d) {return d.colour;})//"#666")
	  .style("stroke-width", 3) //function(d) { return Math.sqrt(d.value); });
	  .on("mouseover", edgeMO)
	  .on("click",edgeClick);
      if (options.directed) {
	  svg.selectAll("line") 
	      .attr("marker-end", "url(#end)");
      }
*/

      var g=svg.append("g"); // to hold all graphic elements

      // draw links as (potentially curved) paths
      var link = g.selectAll(".link")
	  .data(links)
	  .enter().append("path")
	  .attr("class", "links")
	  .style("stroke",link_stroke_colour_function)
	  .style("stroke-width", link_stroke_width_function)
	  .style("fill","none")
	  .on("mouseover", link_mouseover_function)
	  .on("mouseout", link_mouseout_function)
	  .on("click",link_onclick_function);
      if (options.directed) {
	  g.selectAll("path")	  
	      .attr("marker-end", "url(#end)");
// doesn't work on Firefox, not tested elsewhere	  
//	  svg.selectAll("marker")	  
//	      .attr("stroke","context-stroke")
//	      .attr("fill","context-fill");
      }
      
      
      // draw nodes
      var node = g.selectAll(".node")
	  .data(d3.values(nodes)) //*** just nodes?
	  .enter().append("g")
	  .attr("class", "node");

      node.append("circle")
	  .attr("r",node_radius_function)
	  .style("opacity",node_fill_opacity_function)
	  .style("stroke", node_stroke_colour_function)
	  .style("stroke-width", node_stroke_width_function)     
	  .style("fill", node_fill_colour_function)
	  .on("mouseover", node_mouseover_function)
	  .on("mouseout", node_mouseout_function)
	  .on("click", click)
	  .call(d3.drag()
		.on("start", dragstarted)
		.on("drag", dragged)
		.on("end", dragended));
      
      node.append("text")
	  //.attr("class", "nodetext")
	  .attr("dx", 12)
	  .attr("dy", ".35em")
	  .text(function(d) { return d.name })
	  .style("font", options.fontSize + "px " + options.fontFamily)
	  .style("fill","#222")
	  .style("opacity",node_label_opacity_function)
	  .style("pointer-events", "none");

      function dragstarted(d) {
	  if (!d3.event.active) simulation.alphaTarget(0.3).restart();
	  d.fx = d.x;
	  d.fy = d.y;
      }
      
      function dragged(d) {
	  d.fx = d3.event.x;
	  d.fy = d3.event.y;
      }

      function dragended(d) {
	  if (!d3.event.active) simulation.alphaTarget(0);
	  d.fx = null;
	  d.fy = null;
      }      
      
      function zoomed() {
	  g.attr("transform", d3.event.transform);
      }

      /*
    function tick() {
      node.attr("transform", function(d) {
        if(options.bounded){ // adds bounding box
            d.x = Math.max(nodeSize(d), Math.min(width - nodeSize(d), d.x));
            d.y = Math.max(nodeSize(d), Math.min(height - nodeSize(d), d.y));
        }
        
        return "translate(" + d.x + "," + d.y + ")"});
        
      path
        .attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; })
        .attr("x2", function(d) { return d.target.x; })
            .attr("y2", function(d) { return d.target.y; })
	    .attr("d", function(d) {
		var dx = d.target.x - d.source.x,
		    dy = d.target.y - d.source.y,
		    dr = Math.sqrt(dx * dx + dy * dy);
		return "M" + 
		    d.source.x + "," + 
		    d.source.y + "A" + 
		    dr + "," + dr + " 0 0,1 " + 
		    d.target.x + "," + 
		    d.target.y;
	    });	
    }
*/
    function default_node_mouseover() {
	d3.select(this).transition().duration(500)
            .attr("r",function(d){return node_radius_function(d)*1.5;});
	d3.select(this.parentNode).select("text").transition()
            .duration(750)
            .attr("x", 13)
            .style("font-size",options.fontSize*1.5 + "px")
            .style("opacity",1);
    }

    function default_node_mouseout() {
	d3.select(this)
	    .transition()
            .duration(500)
        .attr("r",function(d){return node_radius_function(d);});
      d3.select(this.parentNode).select("text").transition()
            .duration(750)
            .attr("x", 0)
            .style("font-size", options.fontSize + "px") 
            .style("opacity",node_label_opacity_function);
    }
    
    function click(d) {
      return eval(options.clickAction)
    }
    
      // make font-family consistent across all elements
      //    d3.select(el).selectAll('text').style('font-family', options.fontFamily);
      simulation
	  .nodes(nodes)
	  .on("tick", ticked);

      simulation
	  .force("link", d3.forceLink(links).distance(link_distance_function))
	  .force("charge", d3.forceManyBody().strength(options.charge))
	  .force("center", d3.forceCenter(width/2,height/2));
      if (options.collision) {
	  simulation.force("collide",d3.forceCollide().radius(20));
      }
      simulation.alpha(1.0).restart(); // so that network will re-layout if re-instantiated by the shiny/rmarkdown page
      
      function ticked() {
	  // leftover from straight links using line, not path
/*	  link
              .attr("x1", function(d) {
		  var angle = Math.atan2(d.target.y-d.source.y,d.target.x-d.source.x);
		  return d.source.x+node_radius_function(d.source)*Math.cos(angle); })
              .attr("y1", function(d) {
		  var angle = Math.atan2(d.target.y-d.source.y,d.target.x-d.source.x);
		  return d.source.y+node_radius_function(d.source)*Math.sin(angle); })
              .attr("x2", function(d) {
		  var angle = Math.atan2(d.target.y-d.source.y,d.target.x-d.source.x);
		  return d.target.x-node_radius_function(d.target)*Math.cos(angle); })
              .attr("y2", function(d) {
		  var angle = Math.atan2(d.target.y-d.source.y,d.target.x-d.source.x);
		  return d.target.y-node_radius_function(d.target)*Math.sin(angle); });	  
*/
	  link
              .attr("x1", function(d) { return d.source.x; })
              .attr("y1", function(d) { return d.source.y; })
              .attr("x2", function(d) { return d.target.x; })
              .attr("y2", function(d) { return d.target.y; })
	      .attr("d", function(d) {
		  var dr = Math.abs(link_curvature_function(d));
		  var angle = Math.atan2(d.target.y-d.source.y,d.target.x-d.source.x);
		  return "M" + 
		      (d.source.x+node_radius_function(d.source)*Math.cos(angle)) + "," +
		      (d.source.y+node_radius_function(d.source)*Math.sin(angle)) + "A" +
		      dr + "," + dr + " 0 0," + (Math.sign(link_curvature_function(d))+1)/2 + " " + //"1 " + 
		      (d.target.x-(node_radius_function(d.target)+link_stroke_width_function(d)*options.directed)*Math.cos(angle)) + "," + (d.target.y-(node_radius_function(d.target)+link_stroke_width_function(d)*options.directed)*Math.sin(angle));
	      });
	  
	  node.attr("transform", function(d) {
              if(options.bounded){ // adds bounding box
		  d.x = Math.max(node_radius_function(d), Math.min(width - node_radius_function(d), d.x));
		  d.y = Math.max(node_radius_function(d), Math.min(height - node_radius_function(d), d.y));
              }
              return "translate(" + d.x + "," + d.y + ")";
	  });	  
      }      
  },
});
