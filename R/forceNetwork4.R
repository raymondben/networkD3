## to do
## node mouseover, mouseout, click
## label dx,dy,fontsize family,colour( fill), opacity
## update docs and examples


#' Create a D3 JavaScript force directed network graph.
#'
#' @param Links a data frame object with the links between the nodes. It should
#' include the \code{Source} and \code{Target} for each link. These should be
#' numbered starting from 0. An optional \code{Value} variable can be included
#' to specify how close the nodes are to one another.
#' @param Nodes a data frame containing the node id and properties of the nodes.
#' If no ID is specified then the nodes must be in the same order as the Source
#' variable column in the \code{Links} data frame. Currently only a grouping
#' variable is allowed.
#' @param Source character string naming the network source variable in the
#' \code{Links} data frame.
#' @param Target character string naming the network target variable in the
#' \code{Links} data frame.
#' @param Value character string naming the variable in the \code{Links} data
#' frame for how wide the links are.
#' @param NodeID character string specifying the node IDs in the \code{Nodes}
#' data frame.
#' @param height numeric height for the network graph's frame area in pixels.
#' @param width numeric width for the network graph's frame area in pixels.
#' @param fontSize numeric font size in pixels for the node text labels.
#' @param fontFamily font family for the node text labels.
#' @param linkDistance numeric or character string. Either numberic fixed
#' distance between the links in pixels (actually arbitrary relative to the
#' diagram's size). Or a JavaScript function, possibly to weight by
#' \code{Value}. For example:
#' \code{linkDistance = JS("function(d){return d.value * 10}")}.
#' @param charge numeric value indicating either the strength of the node
#' repulsion (negative value) or attraction (positive value).
#' @param zoom logical value to enable (\code{TRUE}) or disable (\code{FALSE})
#' zooming.
#' @param bounded logical value to enable (\code{TRUE}) or disable
#' (\code{FALSE}) the bounding box limiting the graph's extent. See
#' \url{http://bl.ocks.org/mbostock/1129492}.
#' @param clickAction character string with a JavaScript expression to evaluate
#' when a node is clicked.
#' @param link_onclick JS expression
#' @param link_mouseover JS expression, e.g. JS("d3.select(this).style(\"stroke\",\"red\");")
#' @param link_mouseout JS expression
#' @param node_mouseover JS expression
#' @param directed logical value to add arrows to edges
#' @param collision logical value to do collision detection and prevent nodes from overlapping
#' @param node_fill_colour string or JS_EVAL: either (a) a colour (e.g. "#777") to be applied to all nodes, (b) a column name (where this column in the nodes data.frame holds character colour strings), or (c) a JS_EVAL object representing a javascript expression such as JS("d3.scaleOrdinal(d3.schemeCategory10).domain([0,1,2])(d.group)")
#' @param node_fill_opacity string, numeric, or JS_EVAL: either (a) a constant, numeric opacity value (0-1) to be applied to all nodes, (b) a column name (where this column in the nodes data.frame holds an opacity value for each node), or (c) a JS_EVAL object representing a javascript expression such as JS("d.value/100")
#' @param node_stroke_colour string or JS_EVAL: as for \code{node_fill_colour}, but controlling the node stroke colour
#' @param node_stroke_width string, numeric, or JS_EVAL: as for \code{node_fill_opacity} but controlling node stroke width
#' @param node_radius string, numeric, or JS_EVAL: as for \code{node_fill_opacity} but controlling node radius
#' @param link_stroke_width string, numeric, or JS_EVAL: as for \code{node_fill_opacity} but controlling link stroke width
#' @param link_distance string, numeric, or JS_EVAL: as for \code{node_fill_opacity} but controlling the length of each link
#' @param link_stroke_colour string or JS_EVAL: as for \code{node_fill_colour}, but controlling the link stroke colour
#' @param link_curvature string or JS_EVAL: as for \code{node_fill_opacity}, but controlling the curvature of each link. Use a large number for straight lines
#' @param nodel_label_opacity string or JS_EVAL: as for \code{node_fill_opacity}, but controlling the opacity of the node labels
#'
#' @examples
#' # Load data
#' data(MisLinks)
#' data(MisNodes)
#' # Create graph
#' forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              Group = "group", opacity = 0.4, zoom = TRUE)
#'
#' # Create graph with legend and varying node radius
#' forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              Nodesize = "size",
#'              radiusCalculation = "Math.sqrt(d.nodesize)+6",
#'              Group = "group", opacity = 0.4, legend = TRUE)
#'
#' \dontrun{
#' #### JSON Data Example
#' # Load data JSON formated data into two R data frames
#' # Create URL. paste0 used purely to keep within line width.
#' URL <- paste0("https://cdn.rawgit.com/christophergandrud/networkD3/",
#'               "master/JSONdata/miserables.json")
#'
#' MisJson <- jsonlite::fromJSON(URL)
#'
#' # Create graph
#' forceNetwork(Links = MisJson$links, Nodes = MisJson$nodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              Group = "group", opacity = 0.4)
#'
#' # Create graph with zooming
#' forceNetwork(Links = MisJson$links, Nodes = MisJson$nodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              Group = "group", opacity = 0.4, zoom = TRUE)
#'
#'
#' # Create a bounded graph
#' forceNetwork(Links = MisJson$links, Nodes = MisJson$nodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              Group = "group", opacity = 0.4, bounded = TRUE)
#'
#' # Create graph with node text faintly visible when no hovering
#' forceNetwork(Links = MisJson$links, Nodes = MisJson$nodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              Group = "group", opacity = 0.4, bounded = TRUE,
#'              opacityNoHover = TRUE)
#'
#' ## Specify colours for specific edges
#' # Find links to Valjean (11)
#' which(MisNodes == "Valjean", arr = TRUE)[1] - 1
#' ValjeanInds = which(MisLinks == 11, arr = TRUE)[, 1]
#'
#' # Create a colour vector
#' ValjeanCols = ifelse(1:nrow(MisLinks) %in% ValjeanInds, "#bf3eff", "#666")
#'
#' forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              Group = "group", opacity = 0.8, linkColour = ValjeanCols)
#'
#'
#' ## Create graph with alert pop-up when a node is clicked.  You're
#' # unlikely to want to do exactly this, but you might use
#' # Shiny.onInputChange() to allocate d.XXX to an element of input
#' # for use in a Shiny app.
#'
#' MyClickScript <- 'alert("You clicked " + d.name + " which is in row " +
#'        (d.index + 1) +  " of your original R data frame");'
#'
#' forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              Group = "group", opacity = 1, zoom = FALSE,
#'              bounded = TRUE, clickAction = MyClickScript)
#' }
#'

#' @source
#' D3.js was created by Michael Bostock. See \url{http://d3js.org/} and, more
#' specifically for force directed networks
#' \url{https://github.com/mbostock/d3/wiki/Force-Layout}.
#' @seealso \code{\link{JS}}.
#'
#' @export
forceNetwork4 <- function(Links,
                          Nodes,
                          Source,
                          Target,
                          Value,
                          NodeID,
                          height = NULL,
                          width = NULL,
                          node_fill_colour = "#008",
                          node_fill_opacity=1, 
                          node_stroke_colour = "#000",
                          node_stroke_width="1.5 px", 
                          node_radius=15,
                          node_label_opacity=1,
                          node_mouseover="default",
                          node_mouseout="default",
                          link_stroke_width="3 px", 
                          link_stroke_colour="#777",
                          link_distance=50,
                          link_curvature=500,
                          fontSize = 12,
                          fontFamily = "serif",
                          charge = -200,
                          zoom = FALSE,
                          bounded = FALSE,
                          clickAction = NULL,
                          link_onclick = NULL,
                          link_mouseover = NULL,
                          link_mouseout = NULL,
                          directed=FALSE,
                          collision=FALSE)
{

    ## is this needed?
    if (inherits(Links,"tbl_df")) Links <- as.data.frame(Links)
    if (inherits(Nodes,"tbl_df")) Nodes <- as.data.frame(Nodes)

    if (!is.data.frame(Links)) stop("Links must be a data frame class object.")
    if (!is.data.frame(Nodes)) stop("Nodes must be a data frame class object.")

    if (is.character(Links[,Source]) || is.character(Links[,Target])) {
        if (!is.character(Links[,Source]) || !is.character(Links[,Target]))
            stop("Source and Target columns in the links data.frame should be of the same class")
        node_levels <- levels(as.factor(Nodes[,NodeID]))
        Links[,Source] <- as.numeric(factor(Links[,Source],levels=node_levels))-1
        Links[,Target] <- as.numeric(factor(Links[,Target],levels=node_levels))-1
    }
    
    clr_to_js <- function(clr,dat) {
        if (clr %in% names(dat)) {
            ## clr is a column in the data frame
                clr <- sprintf("return d.%s;",clr)
        } else {
            if (inherits(clr,"JS_EVAL")) {
                ## is js expression
                clr <- sprintf("return %s;",as.character(clr))
            } else {
                ## assume is a meaningful string value (e.g. a colour, or line width like "1.5px"
                clr <- sprintf("return '%s';",clr)
            }
        }
        clr
    }
        
    ## node properties
    node_fill_colour <- clr_to_js(node_fill_colour,Nodes)
    node_fill_opacity <- clr_to_js(node_fill_opacity,Nodes)
    node_stroke_colour <- clr_to_js(node_stroke_colour,Nodes)
    node_stroke_width <- clr_to_js(node_stroke_width,Nodes)
    node_radius <- clr_to_js(node_radius,Nodes)

    ## edge properties
    link_stroke_width <- clr_to_js(link_stroke_width,Links)
    link_stroke_colour <- clr_to_js(link_stroke_colour,Links)
    link_curvature <- clr_to_js(link_curvature,Links)
    link_distance <- clr_to_js(link_distance,Links)
    
    names(Links)[names(Links)==Source] <- "source"
    names(Links)[names(Links)==Target] <- "target"
    names(Nodes)[names(Nodes)==NodeID] <- "name"
    if (!missing(Value)) names(Links)[names(Links)==Value] <- "value"

    # create options
    options = list(
        ##NodeID = NodeID,
        ##Group = Group,
        fontSize = fontSize,
        fontFamily = fontFamily,
        clickTextSize = fontSize * 1.5,
        ##linkDistance = linkDistance,
        ##linkWidth = linkWidth,
        charge = charge,
                                        # linkColour = linkColour,
        ##opacity = opacity,
        zoom = zoom,
        ##legend = legend,
        bounded = bounded,
        clickAction = clickAction,
        directed = directed,
        collision = collision,
        node_fill_colour=node_fill_colour,
        node_fill_opacity=node_fill_opacity,
        node_stroke_colour=node_stroke_colour,
        node_stroke_width=node_stroke_width,
        node_radius=node_radius,
        node_label_opacity=node_label_opacity,
        link_stroke_width=link_stroke_width,
        link_stroke_colour=link_stroke_colour,
        link_curvature=link_curvature,
        link_distance=link_distance,
        link_mouseover=as.character(link_mouseover),
        link_mouseout=as.character(link_mouseout),
        link_onclick=as.character(link_onclick),
        node_mouseover=as.character(node_mouseover),
        node_mouseout=as.character(node_mouseout)
    )
    
    # create widget
    htmlwidgets::createWidget(
        name = "forceNetwork4",
        x = list(links = Links, nodes = Nodes, options = options),
        width = width,
        height = height,
        htmlwidgets::sizingPolicy(padding = 10, browser.fill = TRUE),
        package = "networkD3"
    )
}

#' @rdname networkD3-shiny
#' @export
forceNetwork4Output <- function(outputId, width = "100%", height = "500px") {
        shinyWidgetOutput(outputId, "forceNetwork4", width, height,
                          package = "networkD3")
}

#' @rdname networkD3-shiny
#' @export
renderForceNetwork4 <- function(expr, env = parent.frame(), quoted = FALSE) {
        if (!quoted) { expr <- substitute(expr) } # force quoted
        shinyRenderWidget(expr, forceNetwork4Output, env, quoted = TRUE)
}
