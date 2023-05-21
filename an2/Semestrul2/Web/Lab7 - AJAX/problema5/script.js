
function loadDoc(method, url, cFunction) {
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
        if (this.readyState === 4 && this.status === 200) {
            cFunction(this);
        }
    };
    xhttp.open(method, url, true);
    xhttp.send();
}

function loadSosire(xhttp) {
    const statii = $.parseJSON(xhttp.responseText);
    console.log(statii);
    const ul = document.getElementById("sosiri");
    $(ul).empty();
    for(let i = 0; i < statii.length; i++){
        const li = document.createElement("li");
        const oras = statii[i].sosire;
        var txt = document.createTextNode(oras);
        $(li).append(txt);
        $(ul).append(li);
    }
}



function loadFiles(xhttp) {

}

function clickDirectory(directory, txt) {
    const ulChl = document.createElement("ul");
    $(directory).append(ulChl);
    // const txt = $(directory).text();
    // console.log('clicked', txt);
    loadDoc("GET", `files.php?dir=${txt}`, function (xhttp) {
        console.log(xhttp.responseText);
        const files = $.parseJSON(xhttp.responseText);
        console.log(files);
        for(let j = 0; j < files.length; j++){
            const liChl = document.createElement("li");
            var t = files[j];
            $(liChl).append(t);
            $(ulChl).append(liChl);
            liChl.clicked = false;
            $(liChl).click(function () {
                if(this.clicked === false) {
                    this.clicked = true;
                    console.log('click', this.innerHTML);
                    clickDirectory(this, `${txt}\\${this.innerHTML}`);
                }
            })
        }
    });
}

function loadDirectories(xhttp) {
    const ul = document.getElementById("list");
    var dirs = $.parseJSON(xhttp.responseText);
    console.log(dirs);
    for(let i = 0; i < dirs.length; i++){
        const li = document.createElement("li");
        const txt = dirs[i];
        $(li).append(txt);
        $(ul).append(li);
        li.clicked = false;
        $(li).click(function () {
            if(this.clicked === false) {
                this.clicked = true;
                clickDirectory(this, txt);
            }
        });
    }
}


$(document).ready(function(){
    loadDoc("GET", "directory.php", loadDirectories);
});
