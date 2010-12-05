var Genrei = (function($){
  
  function Genrei(options){
    var self = this;
    assert({ val: options.container, type: 'string', required: true });
    assert({ val: options.uri, type: 'string', required: true });
    assert({ val: options.welcomeMessage, type: 'string' });
    self.options = options;
    // Initial values
    self.queuedQuery = null;
    self.cache = {};
    // Create the DOM and setup event handlers
    $(options.container).each(function(){
      var c = $(this);
      // Make the form
      var form = $('<form class="genrei-form"></form>');
      // Choices
      self.chooser = $('<select class="genrei-choice"></select>')
        .append($('<option></option>').val('camxes:flat')
                .text('Outline the Grammar (camxes)'))
        .append($('<option></option>').val('camxes:nested')
                .text('Pretty print the Grammar (camxes)'))
        .append($('<option></option>').val('jbofihe:grammar')
                .text("Outline the Grammar (jbofi'e)"))
        .append($('<option></option>').val('jbofihe:translate')
                .text('Translate'))
        .append($('<option></option>').val('jbofihe:translate-html')
                .text('Translate (Pretty)'))
        .append($('<option></option>').val('jbofihe:translate-latex')
                .text('Translate (LaTeX)'))
        .append($('<option></option>').val('jbofihe:parse-tree')
                .text('Parse Tree'))
        .append($('<option></option>').val('jbofihe:parse-tree-full')
                .text('Parse Tree (Full)'));
      form.append(self.chooser);
      // Default choices
      self.chooser.val('jbofihe:translate');
      self.engine = 'jbofihe';
      self.type = 'translate';
      // Update choice
      self.chooser.change(function(){
        var typ = $(this).val().split(/:/);
        self.engine = typ[0];
        self.type = typ[1];
        self.currentValue = '';
      });
      // Input area
      self.textarea = $('<textarea spellcheck="false" class="genrei-textarea"></textarea>');
      self.textarea.val(options.welcomeMessage);
      form.append(self.textarea);
      c.append(form);
      // Update with cached data
      setInterval(function(){ self.updateFromCache(); },20);
      self.textarea.change(function(){ self.updateFromCache(); });
      // Display area
      self.display = $('<div class="genrei-display"></div>');
      self.textarea.after(self.display);
      // Start query queuing process.
      self.startQueue();
    });
  };

  Genrei.prototype.updateFromCache = function(){
    var self = this;
    var val = self.textarea.val();
    var cached = self.cache[self.engine+':'+self.type+':'+val];
    window.cache = self.cache;
    if (cached) {
      self.currentValue = val;
      if (cached.success) {
        self.renderReply(cached);
        self.textarea.removeClass('genrei-error');
      }
      else self.textarea.addClass('genrei-error');
    } else {
      console.log('No cache!');
    }
  };

  Genrei.prototype.startQueue = function(){
    var self = this;
    setInterval(function(){
      if (!self.querying && self.currentValue != self.textarea.val()) {
        console.log('%o != %o',self.currentValue,self.textarea.val());
        var val = self.textarea.val();
        self.currentValue = val;
        self.queryType = self.type;
        self.querying = true;
        $.ajax({
          method:'POST',
          url: '/genrei/json',
          data: {
            method: self.engine + '-grammar',
            type: self.type,
            q: self.textarea.val()
          },
          dataType: 'json',
          success:function(reply){
            self.cache[self.engine+':'+self.type+':'+val] = reply;
            if (reply.success) {
              self.renderReply(reply);
              self.textarea.removeClass('genrei-error');
            }
            else self.textarea.addClass('genrei-error');
            self.queuedQuery = null;
            self.querying = false;
          }, 
          error:function(){
            self.queuedQuery = null;
            self.querying = false;
          } 
        });
      }
    },100);
  };

  Genrei.prototype.renderReply = function(text){
    var self = this;
    if (self.engine == 'jbofihe') {
      switch (self.queryType) {
      case 'translate':
      case 'translate-latex':
      case 'grammar': self.display.text(text.success); break;
      case 'translate-html': self.display.html(text.success); break;
      case 'parse-tree-full':
      case 'parse-tree': {
        self.display.text(text.success);
        self.display.html(self.display.html().replace(/\n/g,'<br>'));
        break;
      }
      }
    }
    if (self.engine == 'camxes') {
      switch (self.queryType) {
      case 'flat': self.display.text(text.success); break;
      case 'nested': {
        self.display.text(text.success);
        self.display.html('<pre>'+self.display.html().replace(/\n/g,'<br>')
                          +'</pre>');
        break;
      }
      }
    }
  };
  
  function assert(spec){
    if (!spec.required && typeof spec.val == 'undefined') return;
    if (!spec.type) return;
    if (typeof spec.val != spec.type) {
      throw "assert(): Expected type `" +
        spec.type + "', but found `" + typeof spec.val +
        "', with the following value: " + spec.val;
    }
  };

  return Genrei;
})(jQuery);