// Copyright (C) 2007 Damien Katz <damien_katz@yahoo.com>.

// This file is licenced under a Creative Commons Attribution 3.0 Unported
// Licence <http://creativecommons.org/licenses/by/3.0/>.

// A simple class to represent a database. Uses XMLHttpRequest
// to interface with the CouchDB server.

function CouchDB(name) {
  this.name = name
  this.url = "/" + name +"/";
  xhr = CouchDB.new_XMLHttpRequest();

  // Creates the database on the server
  this.createDb = function() {
    xhr.open("PUT", this.url, false);
    xhr.send(null);
    if (xhr.status != 201)
      throw xhr.responseText.parseJSON();
    return xhr.responseText.parseJSON();
  }

  // Deletes the database on the server
  this.deleteDb = function() {
    xhr.open("DELETE", this.url, false);
    xhr.send(null);
    if (xhr.status == 404)
      return false;
    if (xhr.status != 202)
      throw xhr.responseText.parseJSON();
    return xhr.responseText.parseJSON();
  }

  // Save a document to the database
  this.save = function(doc, options) {
    if (doc._id == undefined)
      xhr.open("POST", this.url + encodeOptions(options), false);
    else
      xhr.open("PUT", this.url  + doc._id + encodeOptions(options), false);
    xhr.send(doc.toJSONString());
    if (xhr.status != 201)
      throw xhr.responseText.parseJSON();
    var result = xhr.responseText.parseJSON();
    // set the _id and _rev members on the input object, for caller convenience.
    doc._id = result.id;
    doc._rev = result.rev;
    return result;
  }

  // Open a document from the database
  this.open = function(docId, options) {
    xhr.open("GET", this.url + docId + encodeOptions(options), false);
    xhr.send(null);
    if (xhr.status == 404)
      return null;
    if (xhr.status != 200)
      throw xhr.responseText.parseJSON();
    return xhr.responseText.parseJSON();
  }

  // Deletes a document from the database
  this.deleteDoc = function(doc) {
    xhr.open("DELETE", this.url + doc._id + "?rev=" + doc._rev, false);
    xhr.send(null);
    if (xhr.status != 202)
      throw xhr.responseText.parseJSON();
    var result = xhr.responseText.parseJSON();
    doc._rev = result.rev; //record rev in input document
    doc._deleted = true;
    return result;
  }

  this.bulkSave = function(docs, options) {
    xhr.open("POST", this.url + encodeOptions(options), false);
    xhr.send({_bulk_docs: docs}.toJSONString());
    if (xhr.status != 201)
      throw xhr.responseText.parseJSON();
    return xhr.responseText.parseJSON();
  }

  // Applies the map function to the contents of database and returns the results.
  this.query = function(mapFun, options) {
    xhr.open("POST", this.url + "_temp_view" + encodeOptions(options), false);
    // specify the query language we are using
    xhr.setRequestHeader("content-type", "text/javascript");
    if (typeof(mapFun) != "string")
      mapFun = mapFun.toSource ? mapFun.toSource() : "(" + mapFun.toString() + ")";
    xhr.send(mapFun);
    if (xhr.status != 200)
      throw xhr.responseText.parseJSON();
    return xhr.responseText.parseJSON();
  }

  this.view = function(viewname, options) {
    return this.open("_design/" + viewname, options);
  }

  // gets information about the database
  this.info = function() {
    xhr.open("GET", this.url, false);
    xhr.send(null);
    if (xhr.status != 200)
      throw xhr.responseText.parseJSON();
    return xhr.responseText.parseJSON();
  }

  this.allDocs = function(options) {
    xhr.open("GET", this.url + "_all_docs" + encodeOptions(options), false);
    xhr.send(null);
    if (xhr.status != 200)
      throw xhr.responseText.parseJSON();
    return xhr.responseText.parseJSON();
  }

  // Convert a options object to an url query string.
  // ex: {key:'value',key2:'value2'} becomes '?key="value"&key2="value2"'
  function encodeOptions(options) {
    var buf = []
    if (typeof(options) == "object" && options !== null) {
      for (var name in options) {
        if (!options.hasOwnProperty(name)) continue;
        var value = options[name];
        if (name == "key" || name == "startkey" || name == "endkey") {
          value = toJSON(value);
        }
        buf.push(encodeURIComponent(name) + "=" + encodeURIComponent(value));
      }
    }
    if (!buf.length) {
      return "";
    }
    return "?" + buf.join("&");
  }

  function toJSON(obj) {
    if (obj === null) {
      return "null";
    }
    return obj.toJSONString()
  }
}

CouchDB.allDbs = function() {
  var xhr = CouchDB.new_XMLHttpRequest();
  xhr.open("GET", "/_all_dbs", false);
  xhr.send(null);
  if (xhr.status != 200)
    throw xhr.responseText.parseJSON();
  return xhr.responseText.parseJSON();
}

CouchDB.getVersion = function() {
  var xhr = CouchDB.new_XMLHttpRequest();
  xhr.open("GET", "/", false);
  xhr.send(null);
  if (xhr.status != 200)
    throw xhr.responseText.parseJSON();
  return xhr.responseText.parseJSON().version;
}

CouchDB.replicate = function(source, target) {
  var xhr = CouchDB.new_XMLHttpRequest();
  xhr.open("POST", "/_replicate", false);
  xhr.send({source: source, target: target}.toJSONString());
  if (xhr.status != 200)
    throw xhr.responseText.parseJSON();
  return xhr.responseText.parseJSON();
}

CouchDB.new_XMLHttpRequest = function() {
  if (typeof(XMLHttpRequest) != "undefined") {
    return new XMLHttpRequest();
  } else if (typeof(ActiveXObject) != "undefined") {
    return new ActiveXObject("Microsoft.XMLHTTP");
  }
}
