# # == - Using pry as a debugger ==

Pry.commands.alias_command 'c', 'continue' rescue nil
Pry.commands.alias_command 's', 'step' rescue nil
Pry.commands.alias_command 'n', 'next' rescue nil
Pry.commands.alias_command 'r!', 'reload!' rescue nil
Pry.commands.alias_command 'ep', 'exit-program' rescue nil

# Pry.config.color = true

# Pry.config.theme = "solarized" unless ENV["INSIDE_EMACS"]

# # === CONVENIENCE METHODS ===
# # Stolen from https://gist.github.com/807492
# # Use Array.toy or Hash.toy to get an array or hash to play with
class Array
  def self.toy(n=10, &block)
    block_given? ? Array.new(n,&block) : Array.new(n) {|i| i+1}
  end
end

class Hash
  def self.toy(n=10)
    Hash[Array.toy(n).zip(Array.toy(n){|c| (96+(c+1)).chr})]
  end
end

class Object
  def im
    case self.class
    when Class
      self.public_methods.sort - Object.public_methods
    when Module
      self.public_methods.sort - Module.public_methods
    else
      self.public_methods.sort - Object.new.public_methods
    end
  end
  alias_method :interesting_methods, :im
end

# def time &block
#   require 'benchmark'
#   result = nil
#   timing = Benchmark.measure do
#     result = block.()
#   end
#   puts "It took: #{timing}"
#   result
# end


# begin
#   require "awesome_print"
#   AwesomePrint.pry!
#   AwesomePrint.defaults = {limit: true}
#   Pry.config.commands.command "limit_true" do |*args|
#   AwesomePrint.defaults = {limit: true}
#   end
#   Pry.config.commands.command "limit" do |*args|
#   current_status = AwesomePrint.defaults[:limit]
#   AwesomePrint.defaults = {limit: !current_status}
#   p "Limiting is #{!current_status ? 'ON' : 'OFF'}"
#   end
#   Pry.config.commands.command "limit_false" do |*args|
#   AwesomePrint.defaults = {limit: false}
#   end
# rescue LoadError
# end

# if ENV["INSIDE_EMACS"] # ENV['TERM'] == 'emacs'
  Pry.config.color = false
  Pry.config.pager = false
  Pry.config.auto_indent = false
# end

# snaptrip:
# to avoid #undefined method `cookie_jar` for nil:NilClass
if defined? Draper
  Draper::ViewContext.build!
  store = RequestStore.store[:current_view_context]
  def store.cookies
  {visitor_email_id: 0}
  end
end
