// All the controls for the sparkline graphing, mapped
// to the events we use to track if they have changed,
// and the function to call when that event occurs.
var controls = {
        'type_s': ['onclick', create_swapper('type_s')], 
        'type_d':['onclick', create_swapper('type_d')], 
        'type_i':['onclick', create_swapper('type_i')], 
        'd': ['onchange', controlChanged], 
        'limits': ['onchange', controlChanged], 
        'height': ['onchange', controlChanged],
        'min-m': ['onclick', controlChanged],
        'max-m': ['onclick', controlChanged],
        'last-m': ['onclick', controlChanged],
        'step': ['onchange', controlChanged],
        'width': ['onchange', controlChanged],
        'upper': ['onchange', controlChanged],
        'above-color': ['onchange', controlChanged],
        'below-color': ['onchange', controlChanged],
        'min-color': ['onchange', controlChanged],
        'max-color': ['onchange', controlChanged],
        'last-color': ['onchange', controlChanged]
};

// Each type of curve takes a different set of parameters
parameters_per_type = {
    "smooth" : ['d', 'height', 'limits', 'min-m', 'max-m', 'last-m', 'min-color', 'max-color', 'last-color', 'step'],
    "discrete" : ['d', 'height', 'limits', 'upper', 'above-color', 'below-color', 'width'],
    "impulse" : ['d', 'height', 'limits', 'upper', 'above-color', 'below-color', 'width']
};

// Different controls have different ways of 
// having their values accessed
parameters_accessor = {
  'd': 'value',
  'height': 'value',
  'limits': 'value',
  'min-m': 'checked',
  'max-m': 'checked',
  'last-m': 'checked',
  'step': 'value',
  'width': 'value',
  'upper': 'value',
  'above-color': 'value',
  'below-color': 'value',
  'min-color': 'value',
  'max-color': 'value',
  'last-color': 'value'
}

// Associates the type of sparkline with the div that
// contains the controls specific to it.
var shape_specific_divs = {
   'type_s': 'smooth_specific',
   'type_d': 'discrete_specific',
   'type_i': 'discrete_specific'
};

function controlChanged() {
    var type = "discrete"
    for (shape in shape_specific_divs) {
        if (document.getElementById(shape).checked) {
            type = document.getElementById(shape).value;
        }
    }
    var output_uri = 'spark.cgi?type=' + type;
    var parameters = parameters_per_type[type];
    for (var i=0; i<parameters.length; i++) {
        output_uri = output_uri + "&" + parameters[i] + "=" + document.getElementById(parameters[i])[parameters_accessor[parameters[i]]];
    }
    document.getElementById('output_uri').value = 'http://localhost:8080/' + output_uri;
    document.getElementById('output_img').src = output_uri;

    return true;
}

function create_swapper(choice) {
    return function() {
        for (type in shape_specific_divs) {
            var s = document.getElementById(shape_specific_divs[type]);
            s.style.display = 'none';
        }
        for (type in shape_specific_divs) {
            var s = document.getElementById(shape_specific_divs[type]);
            if (type == choice) {
                s.style.display = 'block';
            } 
        }


        controlChanged();
    }
}

function setup() {
    for (id in controls) {
        document.getElementById(id)[controls[id][0]] = controls[id][1];
    }
    controlChanged();
}

window.onload = setup;

