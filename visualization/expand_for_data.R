library(splitstackshape)


mydf <- data.frame(x = c("a", "b", "q"), 
                   y = c("c", "d", "r"), 
                   count = c(2, 5, 3))
library(data.table)
DT <- as.data.table(mydf)
mydf
expandRows(mydf, "count")
expandRows(DT, "count", drop = FALSE)
expandRows(mydf, count = 3) ## This takes values from the third column!
expandRows(mydf, count = 3, count.is.col = FALSE)
expandRows(mydf, count = c(1, 5, 9), count.is.col = FALSE)
expandRows(DT, count = c(1, 5, 9), count.is.col = FALSE)

DT

library(mirt)
data(LSAT7)
head(LSAT7)
LSAT7full <- expand.table(LSAT7)
head(LSAT7full)

LSAT7full <- expand.table(LSAT7, sample = TRUE)
head(LSAT7full)

##########################################


dat <- data.frame(
  ' ' = rep('&oplus;',2),
  Sr = c(1, 2),
  Description = c("A - B", "X - Y"),
  Details = I(list(list(list(Chromosome = "chr18", SNP = "rs2")), 
                   list(list(Chromosome = "chr19", SNP = "rs3"),
                        list(Chromosome = "chr20", SNP = "rs4")))), 
  check.names = FALSE
)

callback = JS(
  "table.column(1).nodes().to$().css({cursor: 'pointer'});",
  "// Format the nested table into another table",
  "var format = function (d) {",
  "  if (d != null) {",
  "    var result = ('<table class=\"display compact\" id=\"child_' + ",
  "                 ((d[2] + '_' + d[3]).replace(/\\s/g, '_')) +",
  "                 '\">').replace(/\\./g, '_') + '<thead><tr>';", 
  "    for (var col in d[4][0]) {",
  "      result += '<th>' + col + '</th>';",
  "    }",
  "    result += '</tr></thead></table>';",
  "    return result;",
  "  } else {",
  "    return '';",
  "  }",
  "}",
  "var format_datatable = function (d) {",
  "  var dataset = [];",
  "  for (i = 0; i < d[4].length; i++) {",
  "    var datarow = $.map(d[4][i], function (value, index) {",
  "      return [value];",
  "    });",
  "    dataset.push(datarow);",
  "  }",
  "  var subtable = $(('table#child_' + d[2] + '_' + d[3])",
  "                   .replace(/\\./g, '_').replace(/\\s/g, '_')).DataTable({",
  "                     'data': dataset,",
  "                     'autoWidth': true,",
  "                     'deferRender': true,",
  "                     'info': false,",
  "                     'lengthChange': false,",
  "                     'ordering': true,",
  "                     'paging': false,",
  "                     'scrollX': false,",
  "                     'scrollY': false,",
  "                     'searching': false,",
  "                     'sortClasses': false,",
  "                     'columnDefs': [{targets: '_all', className: 'dt-center'}]",
  "                   });",
  "};",
  "table.on('click', 'td.details-control', function () {",
  "  var td = $(this),",
  "      row = table.row(td.closest('tr'));",
  "  if (row.child.isShown()) {",
  "    row.child.hide();",
  "    td.html('&oplus;');",
  "  } else {",
  "    row.child(format(row.data())).show();",
  "    td.html('&CircleMinus;');",
  "    format_datatable(row.data())",
  "  }",
  "});")

datatable(dat, callback = callback, escape = FALSE,
          options = list(
            columnDefs = list(
              list(visible = FALSE, targets = 4),
              list(orderable = FALSE, className = 'details-control', targets = 1)
            )
          ))



######################


library(DT)
iris$Sepal.Width <- 'Verrrrrrrrry Looooooooooooooong Commmmmmment'

iris_upd <- cbind(' ' = '<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_open.png\"/>', iris)

datatable(
  iris_upd, 
  escape = -2,
  options = list(
    columnDefs = list(
      list(visible = FALSE, targets = c(0)),
      list(orderable = FALSE, className = 'details-control', targets = 1),
      list(
        targets = 3,
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 6 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
          "}")
      )
    )
  ),
  callback = JS("
                  table.column(1).nodes().to$().css({cursor: 'pointer'});
                  var format = function(d) {
                  return'<p>' + d[3] + '</p>';
                  };
                  table.on('click', 'td.details-control', function() {
                  var td = $(this), row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                  row.child.hide();
                  td.html('<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_open.png\"/>');
                  } else {
                  row.child(format(row.data())).show();
                  td.html('<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_close.png\"/>');
                  }
                  });"
  ))




####################

library(DT)
datatable(
  cbind(' ' = '<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_open.png\"/>', 
        mtcars), escape = -2,
  options = list(
    columnDefs = list(
      list(visible = FALSE, targets = c(0, 2, 3)),
      list(orderable = FALSE, className = 'details-control', targets = 1)
    )
  ),
  callback = JS("
                table.column(1).nodes().to$().css({cursor: 'pointer'});
                var format = function(d) {
                return '<div style=\"background-color:#eee; padding: .5em;\"> Model: ' +
                d[0] + ', mpg: ' + d[2] + ', cyl: ' + d[3] + '</div>';
                };
                table.on('click', 'td.details-control', function() {
                var td = $(this), row = table.row(td.closest('tr'));
                if (row.child.isShown()) {
                row.child.hide();
                td.html('<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_open.png\"/>');
                } else {
                row.child(format(row.data())).show();
                td.html('<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_close.png\"/>');
                }
                });"
  ))


##############

library(shiny)
library(DT)
ui <- fluidPage(# Application title
  titlePanel("Collapse/Expand table"),
  mainPanel(DTOutput("my_table")))

callback_js <- JS(
  "table.on('click', 'tr.group', function () {",
  "  var rowsCollapse = $(this).nextUntil('.group');",
  "  $(rowsCollapse).toggleClass('hidden');",
  "});"
)

server <- function(input, output) {
  output$my_table <- DT::renderDT({
    datatable(
      mtcars[1:15, 1:5],
      extensions = 'RowGroup',
      options = list(rowGroup = list(dataSrc = 3), pageLength = 20),
      callback = callback_js,
      selection = 'none'
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

#################################


## data
dat <- data.frame(
  Sr = c(1.5, 2.3),
  Description = c("A - B", "X - Y")
)
## details of row 1
subsubdat1 <- data.frame(
  Ref = c("UVW", "PQR"),
  Case = c(99, 999),
  stringsAsFactors = FALSE
)
subdat1 <- data.frame(
  Chromosome = "chr18", 
  SNP = "rs2",
  details = I(list(purrr::transpose(subsubdat1))),
  stringsAsFactors = FALSE
)
subdat1 <- cbind(" " = "&oplus;", subdat1, stringsAsFactors = FALSE)
## details of row 2
subdat2 <- data.frame(
  Chromosome = c("chr19","chr20"), 
  SNP = c("rs3","rs4"), 
  stringsAsFactors = FALSE
)

## merge the row details
subdats <- lapply(list(subdat1, subdat2), purrr::transpose)
## dataframe for the datatable
Dat <- cbind(" " = "&oplus;", dat, details = I(subdats))

## the callback
callback = JS(
  "table.column(1).nodes().to$().css({cursor: 'pointer'});",
  "// Format the nested table into another table",
  "var childId = function(d){",
  "  var tail = d.slice(2, d.length - 1);",
  "  return 'child_' + tail.join('_').replace(/[\\s|\\.]/g, '_');",
  "};",
  "var format = function (d) {",
  "  if (d != null) {",
  "    var id = childId(d);",
  "    var html = ", 
  "          '<table class=\"display compact\" id=\"' + id + '\"><thead><tr>';",
  "    for (var key in d[d.length-1][0]) {",
  "      html += '<th>' + key + '</th>';",
  "    }",
  "    html += '</tr></thead></table>'",
  "    return html;",
  "  } else {",
  "    return '';",
  "  }",
  "};",
  "var rowCallback = function(row, dat, displayNum, index){",
  "  if($(row).hasClass('odd')){",
  "    for(var j=0; j<dat.length; j++){",
  "      $('td:eq('+j+')', row).css('background-color', 'papayawhip');",
  "    }",
  "  } else {",
  "    for(var j=0; j<dat.length; j++){",
  "      $('td:eq('+j+')', row).css('background-color', 'lemonchiffon');",
  "    }",
  "  }",
  "};",
  "var headerCallback = function(thead, data, start, end, display){",
  "  $('th', thead).css({",
  "    'border-top': '3px solid indigo',", 
  "    'color': 'indigo',",
  "    'background-color': '#fadadd'",
  "  });",
  "};",
  "var format_datatable = function (d) {",
  "  var dataset = [];",
  "  var n = d.length - 1;",
  "  for (var i = 0; i < d[n].length; i++) {",
  "    var datarow = $.map(d[n][i], function (value, index) {",
  "      return [value];",
  "    });",
  "    dataset.push(datarow);",
  "  }",
  "  var id = 'table#' + childId(d);",
  "console.log(d);",
  "  if (Object.keys(d[n][0]).indexOf('details') === -1) {",
  "    var subtable = $(id).DataTable({",
  "                     'data': dataset,",
  "                     'autoWidth': true,",
  "                     'deferRender': true,",
  "                     'info': false,",
  "                     'lengthChange': false,",
  "                     'ordering': d[n].length > 1,",
  "                     'paging': false,",
  "                     'scrollX': false,",
  "                     'scrollY': false,",
  "                     'searching': false,",
  "                     'sortClasses': false,",
  "                     'rowCallback': rowCallback,",
  "                     'headerCallback': headerCallback,",
  "                     'columnDefs': [{targets: '_all', className: 'dt-center'}]",
  "                   });",
  "  } else {",
  "    var subtable = $(id).DataTable({",
  "                     'data': dataset,",
  "                     'autoWidth': true,",
  "                     'deferRender': true,",
  "                     'info': false,",
  "                     'lengthChange': false,",
  "                     'ordering': d[n].length > 1,",
  "                     'paging': false,",
  "                     'scrollX': false,",
  "                     'scrollY': false,",
  "                     'searching': false,",
  "                     'sortClasses': false,",
  "                     'rowCallback': rowCallback,",
  "                     'headerCallback': headerCallback,",
  "                     'columnDefs': [{targets: -1, visible: false}, {targets: 0, orderable: false, className: 'details-control'}, {targets: '_all', className: 'dt-center'}]",
  "                   }).column(0).nodes().to$().css({cursor: 'pointer'});",
  "  }",
  "};",
  "table.on('click', 'td.details-control', function () {",
  "  var tbl = $(this).closest('table');",
  "  var td = $(this),",
  "      row = $(tbl).DataTable().row(td.closest('tr'));",
  "  if (row.child.isShown()) {",
  "    row.child.hide();",
  "    td.html('&oplus;');",
  "  } else {",
  "    row.child(format(row.data())).show();",
  "    td.html('&CircleMinus;');",
  "    format_datatable(row.data());",
  "  }",
  "});")

## datatable
datatable(Dat, callback = callback, escape = -2,
          options = list(
            columnDefs = list(
              list(visible = FALSE, targets = ncol(Dat)),
              list(orderable = FALSE, className = 'details-control', targets = 1),
              list(className = "dt-center", targets = "_all")
            )
          ))
