#! /usr/bin/env ruby

require 'getoptlong'
require 'tempfile'

$image_viewer = 'eog'

def usage
   puts "decompiler.rb [--graph] file.dmp"
end

def graph filename
   tmpfile = Tempfile.new 'decompiler'
   system "./decompiler -graph #{filename} | dot -T svg -o #{tmpfile.path}"
   `#{$image_viewer} #{tmpfile.path}`
end


opts = GetoptLong.new(['--disasm', '-d', GetoptLong::NO_ARGUMENT],
                      ['--blocks', '-b', GetoptLong::NO_ARGUMENT],
                      ['--graph',  '-g', GetoptLong::NO_ARGUMENT])

if ARGV.size < 1
   usage
   exit
end

for opt, arg in opts
   case opt
   when '--help'
      usage
      exit
   when '--disasm'
      system "./decompiler -disasm #{ARGV[0]}"
      exit
   when '--blocks'
      system "./decompiler -blocks #{ARGV[0]}"
      exit
   when '--graph'
      graph ARGV[0]
      exit
   end
end
