var Genrei = (function($){
  
  function Genrei(options){
    var self = this;
    assert({ val: options.container, type: 'string', required: true });
    assert({ val: options.uri, type: 'string', required: true });
    self.options = options;
    // Initial values
    self.queuedQuery = null;
    // Create the DOM and setup event handlers
    $(options.container).each(function(){
      var c = $(this);
      // Make the form
      var form = $('<form class="genrei-form"></form>');
      // Options
      self.chooser = $('<select class="genrei-choice"></select>')
        .append($('<option></option>').val('grammar')
                .text('Outline the Grammar'))
        .append($('<option></option>').val('translate')
                .text('Translate'))
        .append($('<option></option>').val('translate-html')
                .text('Translate (Pretty)'))
        .append($('<option></option>').val('translate-latex')
                .text('Translate (LaTeX)'))
        .append($('<option></option>').val('parse-tree')
                .text('Parse Tree'))
        .append($('<option></option>').val('parse-tree-full')
                .text('Parse Tree (Full)'));
      self.chooser.val('translate');
      self.type = 'translate';
      self.chooser.change(function(){
        self.type = $(this).val();
        self.updateDisplay();
      });
      form.append(self.chooser);
      // Input area
      self.textarea = $('<textarea class="genrei-textarea"></textarea>');
      form.append(self.textarea);
      c.append(form);
      // Display area
      self.display = $('<div class="genrei-display"></div>');
      self.textarea.after(self.display);
      // Update the display when necessary
      self.textarea.change(function(){ self.updateDisplay(); });
      self.textarea.keydown(function(){ self.updateDisplay(); });
      setInterval(function(){
        if (self.textarea.val() != self.currentValue) {
          self.currentValue = self.textarea.val();
          self.updateDisplay(self.currentValue);
        }
      },50);
      // Start query queuing process.
      self.startQueue();
    });
  };

  Genrei.prototype.updateDisplay = function(){
    var self = this;
    self.queuedQuery = { text: self.currentValue };
  };

  Genrei.prototype.startQueue = function(){
    var self = this;
    setInterval(function(){
      if (!self.querying && self.queuedQuery != null) {
        self.queryType = self.type;
        self.querying = true;
        $.ajax({
          method:'POST',
          url: '/genrei/json',
          data: {
            method: 'jbofihe-grammar',
            type: self.type,
            q: self.textarea.val()
          },
          dataType: 'json',
          success:function(text){
            self.renderReply(text);
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