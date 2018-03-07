exports.escapeHTML = escapeHTML;
exports.trace = trace; // defined elsewhere


function escapeHTML(x)
{
    return x
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&#039;");
}
