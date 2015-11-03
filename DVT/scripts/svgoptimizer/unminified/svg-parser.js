var defaultStyles = {
    "display": "inline",
    "visibility": "visible",  // Should remove hidden elements or those with opacity 0
    "opacity": "1",
    "enable-background": "accumulate",
    "fill": "#000000",
    "fill-opacity": "1",
    "fill-rule": "nonzero",
    "marker": "none",
    "marker-start": "none",
    "marker-mid": "none",
    "marker-end": "none",
    "stroke": "none",
    "stroke-width": "1",
    "stroke-opacity": "1",
    "stroke-miterlimit": "4",
    "stroke-linecap": "butt",
    "stroke-linejoin": "miter",
    "stroke-dasharray": "none",
    "stroke-dashoffset": "0",
    "stop-opacity": 1,
    "font-anchor": "start",
    "font-style": "normal",
    "font-weight": "normal",
    "font-stretch": "normal",
    "font-variant": "normal"
};

// Styles that can probably be removed
var nonEssentialStyles = {
    "color" : true,
    "display" : true,
    "overflow" : true,
    "fill-rule" : true,
    "clip-rule" : true,
    "stroke-miterlimit" : true
};

var standardAttributes = ["id", "class", "transform", "style"];

// Split a string from a style attribute into a hash of styles
// e.g. style="fill:#269276;opacity:1" => {fill: '#269276', opacity: '1'}
// Also simplify #ff8800 top #f80

var parseStyle = function(styleString) {
    var styles = {};
    var styleArray = styleString.split(/\s*;\s*/);
    
    for (var i in styleArray) {
        var value = styleArray[i].split(/\s*:\s*/);
        
        if (value.length === 2) {
            // Remove unnecessary px
            if (value[0] === 'stroke-width' && value[1].slice(-2) === 'px') {
                value[1] = value[1].slice(0, -2);
            }

            // Simplify colours, e.g. #ffffff -> #fff
            if (value[0] === 'stroke' || value[0] === 'fill') {
                var v = value[1];
                if (v[0] === '#' && v.length === 7 && v[1] === v[2] && v[3] === v[4] && v[5] === v[6]) {
                    value[1] = '#' + v[1]  + v[3] + v[5];
                }
            }

            styles[value[0]] = value[1];
        }
    }
    
    return styles;
}

//Convert a string of digits to an array of floats
var extractDigits = function(digit_string) {
    if (!digit_string) {return;}
    
    var re_digits = /([-+]?[\d\.]+)([eE][-+]?[\d\.]+)?/g;
    var digits = [];
    
    while(digit = re_digits.exec(digit_string)){
        digits.push(parseFloat(digit));
    }
    return digits;
};

// Split a string from a path "d" attribute into a list of letters and values
var parsePath = function(coord_string) {
    var re_commands = /([ACHLMQSTVZ])([-\+\d\.\s,e]*)/gi
    var command_letters = [];
    var command_values = [];

    while(commands = re_commands.exec(coord_string)){
        command_letters.push(commands[1]);
        command_values.push(extractDigits(commands[2]));
    }

    return [command_letters, command_values];
}

// Returns a function that will convert take a number and
// convert it to a string with a specific number of decimal places
var getDecimalPlaceFunction = function(decimal_places) {
    if (!isNaN(parseInt(decimal_places))) {
        var scale = Math.pow(10, decimal_places);
        return function(x) {
            if (isNaN(parseFloat(x))) {
                return x; 
            } else {
                //return "" + Math.round(parseFloat(x) * scale) / scale;
				//EWX fix - when there was a number e.g -0.016 the functioned returned 0 while below return -0
				//return parseFloat(x).toFixed(decimal_places);
				if(parseFloat(x)<0 && (Math.round(parseFloat(x) * scale) / scale)==0){
					return " -"+(Math.round(parseFloat(x) * scale) / scale);
				} else {
					if((Math.round(parseFloat(x) * scale) / scale)<0){
						return " " + (Math.round(parseFloat(x) * scale) / scale);
					} 
					return "" + (Math.round(parseFloat(x) * scale) / scale);
				}
            }
        };
    } else {
        return function(x) { return x; };
    }
};


var SVG_Style_Element = function(tree) {
    this.tree = tree;
    this.data = '';

    this.toString = function () {
        if (!this.tree.useCSS || !this.data) { return ''; }

        var str = this.tree.removeWhitespace ? '<style>' : '<style>\n';
        str += this.data;
        str += this.tree.removeWhitespace ? '</style>' : '\n</style>';
        return str;
    }

    // Empty functions
    this.createCSS = function() {}
    this.createMap = function() {}
    this.toJQueryObject = function(parent) {}
}

var SVG_Element = function(element, tree, parents) {
    this.tree = tree;
    this.tag = element.nodeName;
    this.attributes = {};
    this.style = {};
    this.children = [];
    this.parents = parents;
    this.data = "";
    this.id;
    
    // Keep track of the number of each type of element
    if (!this.tree.element_counts[this.tag]) { this.tree.element_counts[this.tag] = 1; }
    else { this.tree.element_counts[this.tag]++; }
    this.count = this.tree.element_counts[this.tag];
    
    // Add attributes to hash
    // Style attributes have a separate hash
    if (element.attributes) {
        for (var i=0; i<element.attributes.length; i++){
            var attr = element.attributes.item(i);
            
            if (attr.nodeName === 'style') {
                this.styles = parseStyle(attr.nodeValue);
            } else {
                // Should add attributes like 'fill' to style hash
                this.attributes[attr.nodeName] = attr.nodeValue;
            }
        }
        this.id = this.attributes.id;
    }
    
    // Add element styles to tree style counter
    for (var style in this.styles) {
        var style = style + ":" + this.styles[style];
        if (this.tree.styles[style]) {
            this.tree.styles[style].count++
        } else {
            this.tree.styles[style] = {'count': 1, 'display': true};
        }
    }
    
    // If no id, then generatate a new one for look-up
    if (!this.id) {
        var n = 0;
        while (this.tree.id_to_element['id' + n]) { n++; }
        this.id = 'id' + n;
    }
    this.tree.id_to_element[this.id] = this;
    
    // Keep track of classes in case we need to use CSS
    if (this.attributes.class) { this.tree.id_to_element[this.attributes.class] = this; }
    
    // Parse path coordinates
    if (this.tag === "path") {
        var commands = parsePath(this.attributes.d);
        this.command_letters = commands[0];
        this.command_values = commands[1];
    }
    
    // Add children
    for (var i=0; i<element.childNodes.length; i++) {
        var child = element.childNodes[i];
        if (child instanceof Text) {
            // Tag contains text
            if (child.data.replace(/^\s*/, "") !== "") {
                this.data = child.data;
            }
        } else {
            var parent_of_children = this.parents.slice();
            parent_of_children.push(this);
            this.children.push(new SVG_Element(child, tree, parent_of_children));
        }
    }
    
    // Get list of attributes that have not been removed
    this.getUsedAttributes = function() {
        var usedAttributes = {};
        
        for (var attr in this.attributes) {
            // Remove attributes whose namespace has been removed
            if (attr.indexOf(':') !== -1) {
                var ns_attr = attr.split(':');
                if (this.tree.namespaces[ns_attr[0]] === false || 
                   (ns_attr[0] === 'xmlns' && this.tree.namespaces[ns_attr[1]] === false)) {
                    continue;
                }
            }
            
            // Remove ids
            if (this.tree.removeIDs && attr === 'id') continue;
            
            usedAttributes[attr] = this.attributes[attr];
        }
        
        return usedAttributes;
    };
    
    // Get list of attributes that have not been removed
    // Used when showing element data
    this.getUsedStyles = function() {
        if (!this.styles) return [];
        var usedStyles = [];
        
        var useFill = (this.styles['fill'] !== 'none' && this.styles['fill-opacity'] !== '0');
        var useStroke = (this.styles['stroke'] !== 'none' && this.styles['stroke-opacity'] !== '0' && this.styles['stroke-width'] !== '0');
        
        for (var style in this.styles) {
            if (!useFill && style.substr(0, 4) === 'fill') { continue };
            if (!useStroke && style.substr(0, 6) === 'stroke') { continue };
            
            var stylename = style + ":" + this.styles[style];
            if (this.tree.styles[stylename].display) {
                usedStyles.push(style + ":" + this.tree.styleDecimals(this.styles[style]));
            }
        }
        
        if (!useFill) { usedStyles.push('fill:none'); }
        
        return usedStyles.sort();
    };

    // Go through children and combine consecutive paths with the same style
    this.combinePaths = function() {
        var currentStyleString = false;
        var firstChild = false;

        for (var c in this.children) {
            var child = this.children[c];
            child.comb_command_letters = false;
            child.comb_command_values = false;

            if (child.tag === 'path') {
                var styleString = child.getUsedStyles().join(";");

                // Create new variable to hold combine commands
                if (styleString === currentStyleString) {
                    if (!firstChild.comb_command_letters) {
                        firstChild.comb_command_letters = firstChild.command_letters;
                        firstChild.comb_command_values = firstChild.command_values;
                    }
                    // If first letter in second path is m, the convert to M
                    if (child.command_letters[0] === 'm') {
                        firstChild.comb_command_letters.push('M');
                        firstChild.comb_command_values.push(child.command_values[0].slice(0,2));
                        if (child.command_values[0].length > 2) {
                            firstChild.comb_command_letters.push('l');
                            firstChild.comb_command_values.push(child.command_values[0].slice(2));
                        }
                        firstChild.comb_command_letters = firstChild.comb_command_letters.concat(child.command_letters.slice(1));
                        firstChild.comb_command_values = firstChild.comb_command_values.concat(child.command_values.slice(1));
                    } else {
                        firstChild.comb_command_letters = firstChild.comb_command_letters.concat(child.command_letters);
                        firstChild.comb_command_values = firstChild.comb_command_values.concat(child.command_values);
                    }
                    child.combined = true;
                } else {
                    currentStyleString = styleString;
                    firstChild = child;
                }
            } else {
                currentStyleString = false;
                if (child.children) { child.combinePaths(); }
            }
        }
    };

    this.createCSS = function() {
        var styles = this.getUsedStyles();
        if (styles.length > 0) {
            var styleString = styles.join(";");

            if (this.tree.styleClasses[styleString]) {
                this.tree.styleClasses[styleString].push(this);
            } else {
                this.tree.styleClasses[styleString] = [this];
            }
        }

        for (var child in this.children) {
            this.children[child].createCSS();
        }
    }
    
    // Create a string for the 'd' attribute of a path
    this.getPathString = function() {
        var coord_string = "";

        if (this.tree.combinePaths) {
            var command_letters = this.comb_command_letters || this.command_letters;
            var command_values = this.comb_command_values || this.command_values;
        } else {
            var command_letters = this.command_letters;
            var command_values = this.command_values;
        }

        var current_letter;
        for (var i=0; i<command_letters.length; i++) {
			//EWX FIX ADD space between path values
            //coord_string += (command_letters[i] === current_letter) ? " " : command_letters[i];
			if(i!=0){
				coord_string += " "+command_letters[i]+" ";
			} else {
				coord_string += command_letters[i]+" ";
			}
			
            current_letter = command_letters[i];
            
            if (command_values[i]) {
                for (var j=0; j<command_values[i].length; j++) {
                    if (j > 0  && command_values[i][j] >= 0) coord_string += " ";
                    coord_string += this.tree.attrDecimals(command_values[i][j]);
                }
            }
        }
        return coord_string;
    };
    
    // Write a tag and its attributes
    this.writeTag = function(closed) {
        var tag = '<' + this.tag;

        // Write attributes
        for (var attr in this.getUsedAttributes()) {
            tag += ' ' + attr + '="';
            //EWX not chnage decimals for other atributes than path d like in the svg viewBox
			//tag += (this.tag === "path" && attr === "d") ? this.getPathString(this.tree.attrDecimals) : this.tree.attrDecimals(this.attributes[attr]);
			if(this.tag === "path" && attr === "d"){
				tag += this.getPathString(this.tree.attrDecimals);
			} else {
				if(this.tag === "svg" && attr === "viewBox"){
					tag += this.attributes[attr];
				} else {
					tag += this.tree.attrDecimals(this.attributes[attr]);
				}
			}
            tag += '"';
        }
        
        // Write styles
        var styles = this.getUsedStyles();
        if (styles.length > 1) {
            styleString = styles.join(';');
            if (this.tree.useCSS && this.tree.styleClasses[styleString] && this.tree.styleClasses[styleString].length > 1) {
                tag += ' class="' + this.class + '"';
            } else {
                tag += ' style="' + styleString + '"';
            }
        } else if (styles.length === 1) {
            // Write separate attribute instead of style attribute
            var style = styles[0].split(":");
            tag += ' ' + style[0] + '="' + style[1] + '"';
        }
        
        tag += closed ? '/>' : '>';
        return tag;
    };
    
    // Write element name and number (if > 1)
    this.writeLabel = function() {
        var label = this.tag;
        if (this.tree.element_counts[this.tag] > 1) { label += " " + this.count;}
        return label
    }
    
    this.toString = function(depth) {
        if (this.tag.indexOf(':') !== -1 && this.tree.namespaces[this.tag.split(':')[0]] === false) {
            return "";
        }

        if (this.tree.combinePaths && this.combined) { return ""; }

        var depth = depth | 0;
        var indent = this.tree.removeWhitespace ? '' : new Array(depth + 1).join('  ');
        var newline = this.tree.removeWhitespace ? '' : '\n';
        
        // Write data
        var dataString = "";
        if (this.data !== "") {
            if (this.tree.removeWhitespace) {
                dataString += this.data;
            } else {
                dataString += new Array(depth + 2).join('  ') + this.data + "\n";
            }
        }
        
        // Don't write group if it has no attributes, but do write its children
        // Assume g element has no data
        if (this.tag === 'g' && this.tree.removeCleanGroups) {
            var nAttributes = 0;
            for (var attr in this.getUsedAttributes()) { nAttributes++; }
            if (nAttributes === 0) {
                var childString = "";
                jQuery(this.children).each( function(i) {
                    childString += this.toString(depth);
                });
                return childString;
            }
        }
        
        // Write child information
        var childString = "";
        jQuery(this.children).each( function(i) {
            childString += this.toString(depth + 1);
        });
        
        // Put it all together
        var str = "";
        if (childString.length + dataString.length > 0) {
            str = indent + this.writeTag(false);
            str += newline + dataString + childString;
            str += indent + '</' + this.tag + '>';
        } else {
            var nAttributes = 0;
            for (var attr in this.getUsedAttributes()) { nAttributes++; }
            
            // Don't write element if it has no children and no attributes
            if (this.tree.removeEmptyElements && nAttributes === 0) { return ""; }
            str = indent + this.writeTag(true);
        }
        
        return str + newline;
    };
    
    // For creating SVG images of the element
    this.toJQueryObject = function(parent) {
        var $element = jQuery('<' + this.tag + '></' + this.tag + '>');
        $element.attr(this.attributes);
        $element.attr({'onmouseup': 'sendClickToParentDocument(evt)'})
        $element.attr({'style': this.getUsedStyles().join(';') })
        
        jQuery(this.children).each( function(i) {
            $element.append(this.toJQueryObject($element));
        });
        
        parent.append($element);
    };
    
    // For creating the element map
    // Output by adding a div to the output and write children as nested divs
    this.createMap = function(output) {
        if (this.tag.indexOf(':') !== -1 && this.tree.namespaces[this.tag.split(':')[0]] === false) {
            return;
        }
        
        var element_div = jQuery('<div></div>');

        element_div.attr({id: "map-" + this.id});
        element_div.addClass("svg-element-div");
        output.append(element_div);        
        
        var label = this.writeLabel();
        
        if (this.children.length === 0) {
            element_div.text(label);
        } else {
            // Add start tag as separate div
            element_div.append(jQuery('<div></div>').text(label));
            
            // Create div for child elements
            var child_elements = jQuery('<div></div>').addClass("svg-child-element");
            jQuery(this.children).each( function(i) {
                this.createMap(child_elements);
            });
            element_div.append(child_elements);
        }
    };
};

// Wrapper for tree of elements within the SVG
// Keeps track of options for optimisation without actually changing anything
var SVG_Tree = function(svg_string) {
    this.svg_string = svg_string.replace(/^[\s\n]*/, "");    
    var svg_doc = jQuery.parseXML( this.svg_string );
    
    this.id_to_element = {};
    this.class_to_element = {};
    this.element_counts = {};
    this.namespaces = {};
    this.styles = {};
    this.styleClasses = {};
    
    this.attrDecimals = getDecimalPlaceFunction('null');
    this.styleDecimals = getDecimalPlaceFunction('null');
    this.styleType = "single";  // Write styles as a single style attribute
    
    this.removeWhitespace = false;
    this.removeEmptyElements = false;
    this.removeCleanGroups = false;
    this.combinePaths = false;
    this.removeIDs = false;
    this.useCSS = false;
    
    this.root = new SVG_Element(jQuery(svg_doc).children()[0], this, []);
    this.root.children.unshift(new SVG_Style_Element(this));
    
    this.findNamespaces = function() {
        for (attr in this.root.attributes) {
            if (attr.slice(0,6) === 'xmlns:') {
                var ns = attr.split(':')[1];
                this.namespaces[ns] = true;
            }
        }
    };
    this.findNamespaces();

    this.createCSS = function() {
        this.styleClasses = {};
        this.root.createCSS();
        var counter = 0;
        var styleString = '';

        for (var styles in this.styleClasses) {
            var elements = this.styleClasses[styles]
            if (elements.length < 2) { continue; }
			
			//EWX rename default class 's' to 'style'
            var styleName = 'style' + counter;
            var styleList = styles.split(';');

            styleString += '.' + styleName + '{'
            styleString += this.removeWhitespace ? '' : '\n'
            for (var s in styleList) {
                styleString += this.removeWhitespace ? styleList[s] + ';' : '\t' + styleList[s] + ';\n';
            }
            styleString += this.removeWhitespace ? '}' : '}\n'

            for (var element in elements) {
                elements[element].class = styleName;
            }
            counter++;
        }

        this.root.children[0].data = styleString;
        this.useCSS = true;
    };
	
	

    this.toString = function() {
        if (this.useCSS) { this.createCSS(); }
        return this.root.toString();
    };
};

var svg_tree;