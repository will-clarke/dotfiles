# ~/rails_template.rb

gem "foundation-rails"
gem 'activeadmin', github: 'activeadmin'
# gem 'pg'
gem 'haml-rails'

gem_group :development do
gem "annotate", ">= 2.5.0"
gem "awesome_print"
gem "better_errors"
gem "binding_of_caller"
gem "letter_opener"
gem "quiet_assets"
gem "spring"
gem "guard", ">= 2.2.2", :require => false
gem "guard-livereload", :require => false
end

gem_group :development, :test do
gem "capybara"
gem "factory_girl_rails"
gem "fuubar"
gem "rspec-rails", "~> 3.0"
gem "pry-rails"
gem "shoulda-matchers"
end

gem_group :test do
gem "launchy", require: false
gem "valid_attribute"
end

gem_group :production, :staging do
gem "rails_12factor"
  gem "unicorn"
  gem "unicorn-worker-killer"
end

scss = <<-SCSS
@import "foundation_and_overrides";
@import "*";
SCSS
run "echo '#{scss}' >> app/assets/stylesheets/application.scss"
run "rm app/assets/stylesheets/application.css"
run "rm README.rdoc"
run "echo '# #{@app_name.titleize}' >> README.md"
run("bundle install")
generate("rspec:install")
generate("foundation:install --force")
rake("db:create")
rake("rails generate haml:application_layout convert")
# Capybara (there is probably a better way to do this)
run "echo 'require \"capybara/rails\"' >> spec/rails_helper.rb"
# .rspec
run "echo '--format Fuubar' >> .rspec"
git :init
git add: "."
git commit: "-a -m initial"
