# AI Configuration Guide

The teachvatory application uses a configurable AI system that allows you to change AI models and settings without modifying the code.

## Configuration File

AI settings are stored in `inst/golem-config.yml`. The current configuration is:

```yaml
default:
  ai:
    model: "gpt-5"
    provider: "openai"
    temperature: 0.3
```

## Available Settings

### Model
- **Current**: `gpt-5`
- **Options**: Any OpenAI model (e.g., `gpt-4`, `gpt-4o`, `gpt-3.5-turbo`)
- **Purpose**: Specifies which AI model to use for analysis

### Provider
- **Current**: `openai`
- **Options**: `openai`, `anthropic`, `google`, etc. (any provider supported by ellmer)
- **Purpose**: Specifies which AI service to use

### Temperature
- **Current**: `0.3`
- **Range**: `0.0` to `1.0`
- **Purpose**: Controls randomness in AI responses (0 = deterministic, 1 = creative)

## How to Change the Model

1. **Edit the config file**: Open `inst/golem-config.yml`
2. **Update the model**: Change the `model` value under the `ai` section
3. **Restart the application**: Changes take effect on next app restart

Example to use GPT-4:
```yaml
default:
  ai:
    model: "gpt-4"
    provider: "openai"
    temperature: 0.3
```

## Environment-Specific Settings

You can have different AI settings for different environments:

```yaml
default:
  ai:
    model: "gpt-3.5-turbo"  # Faster/cheaper for development
    
production:
  ai:
    model: "gpt-5"  # Best quality for production
    
dev:
  ai:
    model: "gpt-4"  # Balance for development
```

## Testing Configuration

Use these R functions to check your current configuration:

```r
# Check current settings
get_ai_model()        # Returns current model
get_ai_provider()     # Returns current provider  
get_ai_temperature()  # Returns current temperature

# Test the setup
setup_ellmer()        # Tests connection and shows settings
```

## API Key Requirements

Make sure your `.Renviron` file contains:
```
OPENAI_API_KEY=your-actual-api-key-here
```

The AI features will only work with a valid API key for your chosen provider.
