"""Shared help formatting for commands that expose configurable defaults."""

import argparse


class DefaultsHelpFormatter(argparse.ArgumentDefaultsHelpFormatter):
    def _get_help_string(self, action):
        # Optional paths have no literal default; their help explains the
        # derived behavior instead of displaying argparse's internal None.
        if action.default is None or not action.option_strings:
            return action.help
        return super()._get_help_string(action)
