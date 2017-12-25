var hsLexer = /[a-zA-Z][a-zA-Z0-9]+|[\r\n]|./;
var symbols = "->:=()[]";

function escapeHTML(x)
{
    return x
         .replace(/&/g, "&amp;")
         .replace(/</g, "&lt;")
         .replace(/>/g, "&gt;")
         .replace(/"/g, "&quot;")
         .replace(/'/g, "&#039;");
}

function showCall(i)
{
    var t = trace.calls[i];
    var inf = trace.functions[t[""]];
    $source = $("#function-source").empty();
    var source = inf.source;
    while (source != "")
    {
        var res = hsLexer.exec(source);
        if (res == null || res[0].length == 0)
        {
            $source.append(source);
            break;
        }
        else
        {
            if (res[0] == "where")
                $source.append("<span class='hs-keyword'>where</a>");
            else if (symbols.indexOf(res[0]) != -1)
                $source.append("<span class='hs-keyglyph'>" + escapeHTML(res[0]) + "</a>");
            else if (res[0] in t)
                $source.append("<abbr title='" + escapeHTML(trace.variables[t[res[0]]]) + "'>" + escapeHTML(res[0]) + "</abbr>");
            else
                $source.append(res[0]);
            source = source.substr(res[0].length);
        }
    }
    $("#function-variables").empty();
    $("#function-depends").empty();
    var variables = [];
    var depends = [];
    var seenDepends = false;
    for (var s in t)
    {
        if (s == "") continue;
        if (s == "$depends") {
            showDepends(t[s]);
            seenDepends = true;
        }
        else
            variables.push(s + " = " + trace.variables[t[s]]);
    }
    if(!seenDepends)
        $("#function-depends-section").hide();
    variables = variables.sort();
    for (var i = 0; i < variables.length; i++)
        $("#function-variables").append($("<li>" + escapeHTML(variables[i]) + "</li>"));
}

function showDepends(s)
{
    var i;
    for(i in s) {
         var msg = renderCall(s[i]);
         $("#function-depends").append($("<li><a href='javascript:showCall(" + s[i] + ")'>" + escapeHTML(msg) + "</a></li>"));

     }
 }
function renderCall(i)
{
     var t = trace.calls[i];
     var inf = trace.functions[t[""]];
     var words = [];
     words.push(inf.name);
     for (var j = 0; j < inf.arguments.length; j++)
        {
            var v = inf.arguments[j];
            if (v in t)
                words.push(trace.variables[t[v]]);
            else
                words.push("_");
        }
     words.push("=");
     words.push(trace.variables[t[inf.result]]);
     var msg = words.join(" ");
     return msg;
}
function showCalls()
{
    var name = $("#function-drop").val();
    var regex = new RegExp();
    try {
        regex = new RegExp($("#function-text").val());
    } catch (e) {
        // Not a valid regex, just ignore it
    }

    var ul = $("#function-list").empty();
    for (var i = 0; i < trace.calls.length; i++)
    {
        var msg = renderCall(i);
        if (!regex.test(msg)) continue;
        ul.append($("<li><a href='javascript:showCall(" + i + ")'>" + escapeHTML(msg) + "</a></li>"));
    }
}

function init()
{
    var funcNames = [];
    for (var i = 0; i < trace.functions.length; i++)
        funcNames.push(trace.functions[i].name);
    funcNames = funcNames.sort();
    var drop = $("#function-drop");
    for (var i = 0; i < funcNames.length; i++)
        drop.append($("<option>" + escapeHTML(funcNames[i]) + "</option>"));

    $("#function-drop").change(showCalls);
    $("#function-text").change(showCalls).on("input", showCalls);
}

$(function(){
    init();
    showCalls();
    showCall(0);
});
