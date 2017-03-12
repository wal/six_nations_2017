class OptaMatchDataParser

  attr_reader :home_team
  attr_reader :away_team
  attr_reader :opta_document
  attr_reader :home_team_player_match_stats
  attr_reader :away_team_player_match_stats

  def initialize(json_file_name)
    parse_opta_json_file(json_file_name)
  end

  def home_team_name
    home_team['Name']
  end

  def away_team_name
    away_team['Name']
  end

  def player_match_stats
    match_stats = []
    match_stats << get_match_stats_for_team(home_team_player_match_stats, home_team_name, away_team_name)
    match_stats << get_match_stats_for_team(away_team_player_match_stats, away_team_name, home_team_name)

    match_stats.flatten
  end

  private

  def get_match_stats_for_team(team_match_data, team_name, opposition_name)
    team_match_statistics = []

    team_match_data['PlayerLineUp']['MatchPlayer'].each do |player_data|
      player_ref = player_data['@attributes']['PlayerRef']
      player_stats = {}
      player_data['Stat'].each do |player_stat|
        stat_name = player_stat['@attributes']['Type']
        stat_value = player_stat['@value']
        player_stats[stat_name] = stat_value
      end

      team_match_statistics << { player_ref: player_ref,
                                 player_name: player_name(player_ref),
                                 team: team_name,
                                 opposition: opposition_name }.merge(player_stats)
    end

    team_match_statistics
  end

  def player_name(player_ref)
    load_player_records[player_ref][:name]
  end

  def parse_opta_json_file(json_file_name)

    match_data_file = File.read(json_file_name)
    @opta_document = JSON.parse(match_data_file)['OptaFeed']['OptaDocument']
    @home_team = opta_document['Team'].first
    @away_team = opta_document['Team'].last

    @home_team_player_match_stats = opta_document['MatchData']['TeamData'].first
    @away_team_player_match_stats = opta_document['MatchData']['TeamData'].last

    puts "Loaded #{json_file_name} - Home Team: #{home_team_name}, Away Team: #{away_team_name}"
  end

  def load_player_records
    players = {}
    [home_team, away_team].each do |team|

      team_name = team['Name']
      team['Player'].map do |player|
        player_id = player['@attributes']['uID']
        formatted_name = "#{player['PersonName']['Last']}, #{player['PersonName']['First']}"
        players[player_id] =  { name: formatted_name, team: team_name}
      end
    end

    players
  end
end

