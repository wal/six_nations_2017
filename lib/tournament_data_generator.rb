require 'json'
require 'csv'
require_relative 'opta_match_data_parser'

tournament_stats = []

Dir.glob '../data/*.json' do |match_file|
  match_data_parser = OptaMatchDataParser.new match_file
  tournament_stats.concat match_data_parser.player_match_stats
end

CSV.open('../data/tournament.csv', 'wb') do |csvfie|
  csvfie << tournament_stats.first.keys
  tournament_stats.each do |player_match_stats|
    csvfie << player_match_stats.values
  end
end

puts 'Generated new tournament.csv file'