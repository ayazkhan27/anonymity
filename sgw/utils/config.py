"""Configuration loader for SGW. All parameters come from configs/sgw_config.yaml."""

import os
import yaml
from pathlib import Path


def load_config(config_path: str = None) -> dict:
    """Load SGW configuration from YAML file.

    Args:
        config_path: Path to config file. Defaults to configs/sgw_config.yaml
                     relative to project root.

    Returns:
        dict: Configuration dictionary.

    Raises:
        FileNotFoundError: If config file does not exist.
        yaml.YAMLError: If config file is malformed.
    """
    if config_path is None:
        project_root = Path(__file__).parent.parent.parent
        config_path = project_root / "configs" / "sgw_config.yaml"
    else:
        config_path = Path(config_path)

    if not config_path.exists():
        raise FileNotFoundError(f"Config file not found: {config_path}")

    with open(config_path, "r") as f:
        config = yaml.safe_load(f)

    # Fix YAML boolean coercion in overrides (e.g. 'on', 'off', 'yes', 'no')
    if "valence_overrides" in config.get("valence", {}):
        import logging
        logger = logging.getLogger(__name__)
        overrides = config["valence"]["valence_overrides"]
        fixed = {}
        for key, val in overrides.items():
            if not isinstance(key, str):
                logger.warning(f"YAML coerced override key to {type(key).__name__}: "
                              f"{key!r} -> converting to string")
            fixed[str(key).lower()] = float(val)
        config["valence"]["valence_overrides"] = fixed

    return config
