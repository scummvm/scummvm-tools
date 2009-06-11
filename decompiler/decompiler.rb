#! /usr/bin/env ruby

require 'getoptlong'
require 'tempfile'

$image_viewer = 'eog'

def usage
   puts "decompiler.rb [--graph] file.dmp"
end

def graph filename
   tmpfile = Tempfile.new 'decompiler'
   `./decompiler -graph #{filename} | dot -T svg -o #{tmpfile.path}`
   `#{$image_viewer} #{tmpfile.path}`
end


opts = GetoptLong.new(['--graph', '-g', GetoptLong::NO_ARGUMENT])

if ARGV.size < 1
   usage
   exit
end

for opt, arg in opts
   case opt
   when '--help'
      usage
      exit
   when '--graph'
      graph ARGV[0]
      exit
   end
end
