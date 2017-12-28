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

    showCallStack(i);

    $("#function-variables").empty();
    var variables = [];
    var seenDepends = false;

    for (var s in t)
    {
        if (s == "" || s == "$parents" || s == "$depends") continue;
        else
            variables.push(s + " = " + trace.variables[t[s]]);
    }
    variables = variables.sort();
    for (var i = 0; i < variables.length; i++)
        $("#function-variables").append($("<li>" + escapeHTML(variables[i]) + "</li>"));
}

function showCallStack(me)
{
    var t = trace.calls[me];
    var d = t["$depends"];
    if(t["$parents"] == null && d == null)
        $("#function-depends-section").hide();
    else {
        var i;
        var par = me;
        var parents = [];
        var depends = [];
        var msg;
        var these;
        $("#function-depends").empty();
        // parents
        while(true){
            these = trace.calls[par]["$parents"];
            if(these == null || these.Length == 0) {
                break;
            }
            // assumes one parent only
            par = these[0];
            msg = renderCall(par);
            parents.unshift("<li><a href='javascript:showCall(" + par + ")'>" + escapeHTML(msg) + "</a></li>");

        }
        // depends
        for(i in d) {
            msg = renderCall(d[i]);
            depends.push("<li><a href='javascript:showCall(" + d[i] + ")'>" + escapeHTML(msg) + "</a></li>");
        }
        // assembling the call stack. There must be a better way...
        var callstack = $("#function-depends");
        var cursor = callstack;
        var temp;
        for(i in parents) {
            temp = $("<ul>");
            cursor.append($(parents[i]).append(temp));
            cursor = temp;
        }
        temp = $("<ul>");
        cursor = cursor.append($("<li>" + escapeHTML(renderCall(me)) + "</li>").append(temp));
        cursor = temp;
        for(i in depends)
            cursor.append($(depends[i]));
        $("#function-depends-section").show();
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
        if(!funcNames.includes(trace.functions[i].name))
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
