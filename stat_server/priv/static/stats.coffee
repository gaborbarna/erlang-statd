root = exports ? this

init = ->
  unless "WebSocket" of window
    console.log "websockets are not supported."
  else
    connect()

connect = ->
  wsHost = "ws://" + window.location.host + "/websocket"
  root.websocket = new WebSocket(wsHost)
  root.plots = {}
  root.datas = {}
  console.log "connecting to: " + wsHost
  root.websocket.onopen = (evt) ->
    onOpen evt
  root.websocket.onclose = (evt) ->
    onClose evt
  root.websocket.onmessage = (evt) ->
    onMessage evt
  root.websocket.onerror = (evt) ->
    onError evt

onOpen = (evt) ->
  console.log "connected"

onClose = (evt) ->
  console.log "disconnected"

onError = (evt) ->
  console.log "error occured"

onMessage = (evt) ->
  obj = jQuery.parseJSON(evt.data)
  commands =
    refresh_nodes: refresh_nodes
    stats_init: stats_init
    stat_info: stat_info
  commands[obj.command] obj.params

stat_info = (params) ->
  stat_id = compose_id(params.node, params.name)
  stat_div = $("#" + stat_id)
  if stat_div.length
    params.val[0] = new Date(params.val[0] / 1000)
    plot = root.plots[stat_id]
    series = root.datas[stat_id]
    series.data.push params.val
    series.data.shift()
    plot.setData [series]
    plot.setupGrid()
    plot.draw()
    
compose_id = (node, resource) ->
  "id_" + CryptoJS.MD5(node + resource)

compose_label = (node, resource, suffix) ->
  node + ": " + resource + " (" + suffix + ")"

stats_init = (params) ->
  load_resource params.node, params.resource, params.stats, params.suffix

set_close_callback = (stat_div, node, resource) ->
  stat_div.find(".btn-close").on "click", (e) ->
    stat_div.remove()
    remove_resource node, resource

load_resource = (node, resource, stats, suffix) ->
  stat_id = compose_id(node, resource)
  if $("#" + stat_id).length is 0
    stat_div = add_resource_div(node, resource)
    label = compose_label(node, resource, suffix)
    ret = draw_plot(stat_div, stats, label)
    set_close_callback stat_div, node, resource
    root.plots[stat_id] = ret.plot
    root.datas[stat_id] = ret.data

set_resize_callback = (stat_div, container, plot) ->
  stat_div.resize ->
    container.css "height", stat_div.height() - 30
    plot.resize()
    plot.setupGrid()
    plot.draw()

add_resource_div = (node, resource) ->
  tpl_stat = $("#tpl_resource_container > :first-child").clone()
  tpl_stat.attr "id", compose_id(node, resource)
  $("div.stat-container").append tpl_stat
  tpl_stat

draw_plot = (stat_div, stats, label) ->
  stats = _.map(stats, (n) ->
    [new Date(n[0] / 1000), n[1]]
  )
  data =
    label: label
    lines: {show: true, fill: true}
    points: {show: true}
    data: stats
  options = xaxis:
    mode: "time"
    timeformat: "%H:%M:%S"
    minTickSize: [5, "second"]
  container = stat_div.find("div.plot-container")
  plot = $.plot(container, [data], options)
  set_resize_callback stat_div, container, plot
  plot.setupGrid()
  plot.draw()
  {plot: plot, data: data}

register_resource = (node, resource) ->
  msg =
    command: "register_resource"
    params:
      node: node
      resource: resource
  root.websocket.send JSON.stringify(msg)

remove_resource = (node, resource) ->
  msg =
    command: "remove_resource"
    params:
      node: node
      resource: resource
  root.websocket.send JSON.stringify(msg)

set_register_callback = (resource_div, node, resource) ->
  resource_div.find("a.resource-link").on "click", (e) ->
    register_resource node, resource

refresh_nodes = (nodes) ->
  $("#toplevel ul").append _.map(nodes, (val, node) ->
    tpl_node = $("#tpl_node_menu > :first-child").clone()
    tpl_node.find("span.node-name").html node
    tpl_node.find("ul.submenu").append _.map(val, (res) ->
      tpl_resource = $("#tpl_resource_menu > :first-child").clone()
      set_register_callback tpl_resource, node, res
      tpl_resource.find("span.resource-name").html res
      tpl_resource
    )
    tpl_node
  )

$(document).ready init