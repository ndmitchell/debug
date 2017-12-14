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
    var t = traceCalls[i];
    var inf = traceFunctions[t[""]];
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
                $source.append("<abbr title='" + escapeHTML(traceVariables[t[res[0]]]) + "'>" + escapeHTML(res[0]) + "</abbr>");
            else
                $source.append(res[0]);
            source = source.substr(res[0].length);
        }
    }
    $("#function-variables").empty();
    var variables = [];
    for (var s in t)
    {
        if (s == "") continue;
        variables.push(s + " = " + traceVariables[t[s]]);
    }
    variables = variables.sort();
    for (var i = 0; i < variables.length; i++)
        $("#function-variables").append($("<li>" + escapeHTML(variables[i]) + "</li>"));
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
    for (var i = 0; i < traceCalls.length; i++)
    {
        var t = traceCalls[i];
        var inf = traceFunctions[t[""]];
        if (name != "(All)" && name != inf.name) continue;
        var words = [];
        words.push(inf.name);
        for (var j = 0; j < inf.arguments.length; j++)
        {
            var v = inf.arguments[j];
            if (v in t)
                words.push(traceVariables[t[v]]);
            else
                words.push("_");
        }
        words.push("=");
        words.push(traceVariables[t[inf.result]]);
        var msg = words.join(" ");
        if (!regex.test(msg)) continue;
        ul.append($("<li><a href='javascript:showCall(" + i + ")'>" + escapeHTML(msg) + "</a></li>"));
    }
}

function init()
{
    var funcNames = [];
    for (var i = 0; i < traceFunctions.length; i++)
        funcNames.push(traceFunctions[i].name);
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
