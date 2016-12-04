#' Create a D3 JavaScript force directed network graph, using v4 of the D3 library.
#'
#' @param Links a data frame object with the links between the nodes. It should
#' include the \code{Source} and \code{Target} for each link. These can be 
#' numbered starting from 0 (as with the \code{forceNetwork} function.
#' Alternatively, they can be strings (node identifiers) and will be converted to
#' numeric indexes internally.
#' @param Nodes a data frame containing the node id and properties of the nodes.
#' @param Source character string naming the network source variable in the
#' \code{Links} data frame.
#' @param Target character string naming the network target variable in the
#' \code{Links} data frame.
#' @param NodeID character string specifying the node IDs in the \code{Nodes}
#' data frame.
#' @param height numeric height for the network graph's frame area in pixels.
#' @param width numeric width for the network graph's frame area in pixels.
#' @param fontSize numeric font size in pixels for the node text labels.
#' @param fontFamily font family for the node text labels.
#' @param charge numeric value indicating either the strength of the node
#' repulsion (negative value) or attraction (positive value).
#' @param zoom logical value to enable (\code{TRUE}) or disable (\code{FALSE})
#' zooming.
#' @param bounded logical value to enable (\code{TRUE}) or disable
#' (\code{FALSE}) the bounding box limiting the graph's extent. See
#' \url{http://bl.ocks.org/mbostock/1129492}.
#' @param node_onclick JS expression to evaluate when a node is clicked.
#' @param node_mouseover JS expression
#' @param node_mouseout JS expression
#' @param link_onclick JS expression
#' @param link_mouseover JS expression, e.g. JS("d3.select(this).style(\"stroke\",\"red\");")
#' @param link_mouseout JS expression
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
#' @param node_label_opacity string or JS_EVAL: as for \code{node_fill_opacity}, but controlling the opacity of the node labels
#' @param node_label_dx numeric or JS_EVAL: x-offset of the label with respect to the node
#' @param node_label_dy numeric or JS_EVAL: y-offset of the label with respect to the node
#' @param node_label_colour string or JS_EVAL: as for \code{node_fill_colour}, but controlling the node label colour
#'
#' @examples
#' ## create dummy data
#' nodes <- data.frame(name=c("Abacetus simplex","Brachidius crassicornis","Catadromus goliath","Darodilia robusta"),N=c(100,120,400,30),some_property=runif(4)*10+10,colour=c("#FF0000","#00FF00","#0000FF","#FFFF00"),stringsAsFactors=FALSE)
#' links <- data.frame(from=c("Abacetus simplex","Brachidius crassicornis","Abacetus simplex","Darodilia robusta"),to=c("Brachidius crassicornis","Catadromus goliath","Catadromus goliath","Catadromus goliath"),edge_width="3 px",edgevar=c(.3,.3,.1,2),colour=c("#555"),stringsAsFactors=FALSE)
#' 
#' forceNetwork4(Links=links,Nodes=nodes,Source="from",Target="to",NodeID="name",node_radius=JS("Math.sqrt(d.N)*2+10"),node_fill_opacity=0.75,bounded=FALSE,zoom=TRUE,directed=TRUE,node_fill_colour="colour")
#' 
#' @source
#' D3.js was created by Michael Bostock. See \url{http://d3js.org/} and, more
#' specifically for force directed networks. \code{forceNetwork4} uses D3 v4, see:
#' \url{https://github.com/d3/d3/blob/master/API.md#forces-d3-force}.
#' @seealso \code{\link{JS}}.
#'
#' @export
forceNetwork4 <- function(Links,
                          Nodes,
                          Source,
                          Target,
                          NodeID,
                          height = NULL,
                          width = NULL,
                          node_fill_colour = "#008",
                          node_fill_opacity=1, 
                          node_stroke_colour = "#000",
                          node_stroke_width="1.5 px", 
                          node_radius=15,
                          node_label_opacity=1,
                          node_label_dx=20,
                          node_label_dy=10,
                          node_label_colour="#222",
                          node_onclick=NULL,
                          node_mouseover="default",
                          node_mouseout="default",
                          link_stroke_width=3,
                          link_stroke_colour="#777",
                          link_distance=150,
                          link_curvature=500,
                          fontSize = 12,
                          fontFamily = "serif",
                          charge = -200,
                          zoom = FALSE,
                          bounded = FALSE,
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

    ## label properties
    node_label_dx <- clr_to_js(node_label_dx,Nodes)
    node_label_dy <- clr_to_js(node_label_dy,Nodes)
    node_label_colour <- clr_to_js(node_label_colour,Nodes)
    
    names(Links)[names(Links)==Source] <- "source"
    names(Links)[names(Links)==Target] <- "target"
    names(Nodes)[names(Nodes)==NodeID] <- "name"
    ##if (!missing(Value)) names(Links)[names(Links)==Value] <- "value"
    Links$value <- 0 ## dummy. Don't understand why this is still needed. It has no effect

    # create options
    options = list(
        fontSize = fontSize,
        fontFamily = fontFamily,
        charge = charge,
        zoom = zoom,
        bounded = bounded,
        directed = directed,
        collision = collision,
        node_fill_colour=node_fill_colour,
        node_fill_opacity=node_fill_opacity,
        node_stroke_colour=node_stroke_colour,
        node_stroke_width=node_stroke_width,
        node_radius=node_radius,
        node_label_opacity=node_label_opacity,
        node_label_dx=node_label_dx,
        node_label_dy=node_label_dy,
        node_label_colour=node_label_colour,
        link_stroke_width=link_stroke_width,
        link_stroke_colour=link_stroke_colour,
        link_curvature=link_curvature,
        link_distance=link_distance,
        link_mouseover=as.character(link_mouseover),
        link_mouseout=as.character(link_mouseout),
        link_onclick=as.character(link_onclick),
        node_onclick=as.character(node_onclick),
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
