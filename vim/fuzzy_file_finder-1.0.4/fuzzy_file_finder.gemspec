# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = %q{fuzzy_file_finder}
  s.version = "1.0.4"

  s.required_rubygems_version = Gem::Requirement.new(">= 1.2") if s.respond_to? :required_rubygems_version=
  s.authors = ["Jamis Buck"]
  s.date = %q{2009-12-11}
  s.description = %q{an implementation of TextMate's cmd-T search functionality}
  s.email = %q{jamis@jamisbuck.org}
  s.extra_rdoc_files = ["lib/fuzzy_file_finder.rb", "README.rdoc"]
  s.files = ["lib/fuzzy_file_finder.rb", "LICENSE", "Manifest", "README.rdoc", "fuzzy_file_finder.gemspec", "Rakefile"]
  s.homepage = %q{}
  s.rdoc_options = ["--line-numbers", "--inline-source", "--title", "Fuzzy_file_finder", "--main", "README.rdoc"]
  s.require_paths = ["lib"]
  s.rubyforge_project = %q{fuzzy_file_finder}
  s.rubygems_version = %q{1.3.5}
  s.summary = %q{an implementation of TextMate's cmd-T search functionality}

  if s.respond_to? :specification_version then
    current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
    s.specification_version = 3

    if Gem::Version.new(Gem::RubyGemsVersion) >= Gem::Version.new('1.2.0') then
    else
    end
  else
  end
end
