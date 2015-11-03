// UDMv4.5 // You Are Here extension v1.2 //
/***************************************************************\

  ULTIMATE DROP DOWN MENU Version 4.5 by Brothercake
  http://www.udm4.com/

\***************************************************************/

/***************************************************************\
 * Set you are here parameters
\***************************************************************/

var youAreHere=[
	"index.htm",		// default page name [eg "index.php", "default.html" etc]
	"You are here: ",	// add text to here-page title ["text"|"none"]
	"You're in this branch: ", // add text to here-branch title ["text"|"none"]
	"before",		// where to add title text ["before"|"after"]
	"no",			// open here-branch menus automatically ["yes"|"no"]
	];

/***************************************************************\
\***************************************************************/
var yah=new Object;yah.addToTitle=function(titleNode,titleText){yah.iText='';yah.nodes=titleNode.childNodes;yah.nodesLen=yah.nodes.length;for(i=0; i<yah.nodesLen; i++){if(yah.nodes[i].nodeType==3){yah.iText=yah.nodes[i].nodeValue;break;}}yah.title=(titleNode.title=='') ? yah.iText : titleNode.title;yah.title=(youAreHere[3]=='before') ? titleText + yah.title : yah.title + titleText;titleNode.title=yah.title;};function compareNumbers(a,b){return b[0]-a[0];};if(typeof window.onload=='function'){um.yon=onload;window.onload=function(){um.yon();prepFind();};}else{window.onload=prepFind;}function prepFind(){var n=0,t=setInterval(function(){if(typeof um.n!='undefined'&&um.ready){clearInterval(t);findHere();}else if(++n>=75){clearInterval(t);}},200);};function findHere(){var tree=document.getElementById('udm');if(!tree){return false;}yah.uri=top.document.location.href.replace('https://','http://');yah.uri=yah.uri.replace(youAreHere[0],'');yah.uri=yah.uri.replace(/,/g,'%2C');yah.matches=[];yah.links=tree.getElementsByTagName('a');yah.linksLen=yah.links.length;for(var i=0; i<yah.linksLen; i++){yah.href=yah.links[i].href.replace('https://','http://');if(yah.href&&yah.href!=um.jv&&!/[a-z]+\:\/\//.test(yah.href)){yah.matches=[];break;}yah.href=yah.href.replace(youAreHere[0],'');yah.href=yah.href.replace(/,/g,'%2C');if(yah.href!=''&&yah.href!=um.jv&&yah.uri.indexOf(yah.href)!=-1){yah.matches[yah.matches.length]=yah.links[i];}}yah.matchesLen=yah.matches.length;if(yah.matchesLen < 1) {return false;}yah.probs=[];for(i=0; i<yah.matchesLen; i++){yah.href=yah.matches[i].href.replace('https://','http://');yah.hrefLen=yah.href.length;yah.probs[i]=[0,yah.href];for(var j=0; j<yah.hrefLen; j++){if(yah.href.charAt(j)==yah.uri.charAt(j)){yah.probs[i][0]++;}}}yah.probs.sort(compareNumbers);yah.href=yah.probs[0][1];for(i=0; i<yah.linksLen; i++){yah.linkref=yah.links[i].href.replace('https://','http://');if(yah.linkref==yah.href){if(youAreHere[1]!='none'){yah.addToTitle(yah.links[i],youAreHere[1]);}applyHereClass(yah.links[i]);}}return true;};function applyHereClass(link){link.style.zIndex=um.e[6]+=2;yah.cname=um.es(link.className);(yah.cname=='')?link.className='udmY':link.className+=' udmY';if(youAreHere[2]!='none'){if(link.title.indexOf(youAreHere[1])==-1){yah.addToTitle(link,youAreHere[2]);}}yah.ppc=um.es(um.gp(link).parentNode.className);yah.isNav=yah.ppc=='udm';if(youAreHere[4]=='yes'&&!yah.isNav){yah.pm=um.gp(link).parentNode;yah.pm.style.height='auto';yah.pm.style.overflow='visible';yah.pm.style.left='auto';yah.pm.style.top='0';if(um.ep){yah.pm.style.position='static';}um.xd(yah.pm);if(!um.mie&&um.e[89]!='none'){um.kl=yah.pm.childNodes.length;i=0;do{um.tn=yah.pm.childNodes.item(i);um.nn=um.vn(um.tn.nodeName).toLowerCase();if(um.nn=='li'){um.ar=um.n.ga(um.gc(um.tn));if(um.ar!=null){um.n.wp(um.ar,um.tn,um.e[70],0,0);}}i++;}while(i<um.kl);}um.sh=null;if(!um.ns&&um.e[58]!='none'){um.n.hl(yah.pm);um.xd(um.sh);}if(um.wie55&&(um.e[13]=='default'||um.e[13]=='iframe')){um.n.il(yah.pm);}if(um.hz){um.n.ts('hidden');}um.xv(yah.pm);}yah.arrow=(!um.mie&&typeof um.n.ga=='function')?um.n.ga(link):null;if(yah.arrow!=null){if(um.s||um.k){yah.arrow=yah.arrow.firstChild;}if((yah.isNav&&um.ni)||(!yah.isNav&&um.mi)){yah.pic=um.es(um.gp(link).className);yah.arrow.src=um.baseSRC+((yah.isNav)?um.e[46]:(typeof um.w[yah.pic]!=um.un)?um.w[yah.pic][24]:um.e[90]);}}if(!yah.isNav){yah.link=um.gc(um.gp(link).parentNode.parentNode);applyHereClass(yah.link);}};
