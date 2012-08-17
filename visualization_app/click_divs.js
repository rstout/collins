$(document).ready(function(){
    $(".node").click(function(){
        window.open('http://tumblr.github.com/platform/collinsTutorial/out/index.html');
    });

    $(".node").mousedown(function(){
        clearTimeout(this.downTimer);
        this.downTimer = setTimeout(function() {
            alert('Identify Theoretically Activated');
        }, 1000); 
    }).mouseup(function(){
        clearTimeout(this.downTimer);
        }); 
});
