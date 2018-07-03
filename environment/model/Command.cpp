#include <memory>

#include "BotCommandError.hpp"
#include "BotCommunicationError.hpp"
#include "Command.hpp"
#include "JsonError.hpp"

#include "util.hpp"

/** The JSON key for command type. */
constexpr auto JSON_TYPE_KEY = "type";
/** The JSON key for entity X location. */
constexpr auto JSON_ENTITY_X_KEY = "entity_x";
/** The JSON key for entity Y location. */
constexpr auto JSON_ENTITY_Y_KEY = "entity_y";
/** The JSON key for direction. */
constexpr auto JSON_DIRECTION_KEY = "direction";

namespace hlt {

/**
 * Convert a Command to JSON format.
 * @param[out] json The output JSON.
 * @param command The command to convert.
 */
void to_json(nlohmann::json &json, const Command &command) { command->to_json(json); }

/**
 * Convert an encoded Command from JSON format.
 * @param json The JSON.
 * @param[out] command The converted command.
 */
void from_json(const nlohmann::json &json, Command &command) {
    // The type field determines the Command subclass that will be instantiated.
    const std::string &type = json.at(JSON_TYPE_KEY);
    if (type == MoveCommand::COMMAND_TYPE_NAME) {
        command = std::make_unique<MoveCommand>(json);
    } else {
        throw JsonError(json);
    }
}

/**
 * Read a Command from bot serial format.
 * @param istream The input stream.
 * @param[out] command The command to read.
 * @return The input stream.
 */
std::istream &operator>>(std::istream &istream, Command &command) {
    // Read one character corresponding to the type, and dispatch the remainder based on its value.
    char command_type;
    if (istream >> command_type) {
        switch (command_type) {
        case MoveCommand::COMMAND_TYPE_SHORT:
            dimension_type entity_x, entity_y;
            Direction direction;
            istream >> entity_x >> entity_y >> direction;
            command = std::make_unique<MoveCommand>(entity_x, entity_y, direction);
            break;
        default:
            throw BotCommunicationError(to_string(command_type));
        }
    }
    return istream;
}

/**
 * Convert a MoveCommand to JSON format.
 * @param[out] json The JSON output.
 */
void MoveCommand::to_json(nlohmann::json &json) const {
    json = {{JSON_TYPE_KEY,      MoveCommand::COMMAND_TYPE_NAME},
            {JSON_ENTITY_X_KEY,  entity_x},
            {JSON_ENTITY_Y_KEY,  entity_y},
            {JSON_DIRECTION_KEY, direction}};
}

/**
 * Create MoveCommand from JSON.
 * @param json The JSON.
 */
MoveCommand::MoveCommand(const nlohmann::json &json) :
        entity_x(json.at(JSON_ENTITY_X_KEY)),
        entity_y(json.at(JSON_ENTITY_Y_KEY)),
        direction(json.at(JSON_DIRECTION_KEY)) {}

/**
 * Cause the move to act on the Map.
 * @param map_transaction The Map transaction to act on.
 * @param player_transaction The Player transaction issuing the command.
 */
void MoveCommand::act_on_map(Map::Transaction &map_transaction, Player::Transaction &player_transaction) const {
    auto location = std::make_pair(entity_x, entity_y);
    const auto &player = player_transaction.player;
    if (player.find_entity(location) == nullptr) {
        throw BotCommandError("Attempt by player " + std::to_string(player.player_id) + " to move unowned entity");
    } else {
        auto destination = location;
        map_transaction.map.move_location(destination, direction);
        player_transaction.move_entity(location, destination);
        map_transaction.move_entity(player, location, destination);
    }
}

}