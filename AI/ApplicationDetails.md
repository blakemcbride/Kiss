# Application Details Template for KISS Framework

## Overview
[Provide a brief description of your application's purpose and main functionality]

**Example:**
> This application manages [primary entities] and provides [key features] for [target users].

## Technical Requirements

### Framework Foundation
- Built on the KISS framework (see `Kiss-Analysis.md` for detailed framework documentation)
- Review `Kiss-Analysis.md` before starting development
- Database schema should be defined in SQL files (e.g., `schema.sql`)

### Backend Implementation
- **Preferred Language**: [Specify: Groovy, Java, or Lisp]
- **Service Location**: `src/main/backend/services/`
- **Naming Convention**: [e.g., EntityNameCrud.groovy, ServiceName.java]
- **Shared Utilities**: Place in `src/main/precompiled/` for cross-service access

### Frontend Implementation
- **Screen Location**: `src/main/frontend/screens/`
- **Structure**: Each screen in its own directory with `.html` and `.js` files
- **Components**: Use KISS framework custom HTML tags (see Kiss-Analysis.md)
- **Main Navigation**: Update `Framework/Framework.html` and `Framework/Framework.js`

## Database Schema

### Table: [table_name]
```sql
-- Provide CREATE TABLE statement or describe structure
-- Include primary keys, foreign keys, constraints, defaults
-- Note any special column types or rules
```

**Example Structure:**
- `id` (type) - Primary key, [generation strategy]
- `name` (varchar) - [Description, constraints]
- `created_date` (timestamp) - Default: CURRENT_TIMESTAMP
- Foreign keys and relationships

[Repeat for each table]

## Application Features

### Feature/Module: [Name]
**Description**: [What this feature does]

**Operations**:
- **List/View**: [Display requirements, filters, sorting]
- **Create/Add**: [Required fields, validation rules, defaults]
- **Update/Edit**: [Editable fields, restrictions]
- **Delete**: [Cascade rules, confirmation requirements]

**Business Rules**:
- [List any special logic or validation]
- [Relationships between entities]
- [Calculated fields or derived data]

[Repeat for each major feature]

## UI/UX Guidelines

### Design Principles
- [Visual style preferences]
- [Branding requirements]
- [Responsive design needs]

### Layout Standards
- **Navigation**: [Menu structure, placement]
- **Forms**: [Field organization, validation display]
- **Grids**: [Column specifications, sorting, filtering]
- **Popups**: [Sizing guidelines, when to use]

### User Interaction Patterns
- **CRUD Operations**: [Button placement, confirmation dialogs]
- **Error Handling**: [How to display errors, validation messages]
- **Success Feedback**: [Notifications, redirects]

## Development Guidelines

### Code Style Preferences
- [Language-specific preferences, e.g., "No parentheses around single statements" for Groovy]
- [Naming conventions for variables, methods, classes]
- [Comment style and documentation requirements]

### Database Best Practices
- [How to handle auto-generated fields]
- [Preferred data access patterns]
- [Transaction boundaries]

### Service Method Pattern
```groovy
// Example service method signature
void methodName(JSONObject injson, JSONObject outjson, 
                Connection db, ProcessServlet servlet) {
    // Pattern for implementation
}
```

### Frontend Development Pattern
- [Component access pattern, e.g., using $$('id')]
- [AJAX call patterns]
- [Error handling in JavaScript]
- [Grid refresh after operations]

## Special Requirements

### Authentication & Security
- [User roles and permissions]
- [Session management requirements]
- [Data access restrictions]

### Data Validation Rules
- [Field-level validation]
- [Business rule validation]
- [Cross-field dependencies]

### Integration Points
- [External systems]
- [API requirements]
- [File imports/exports]

## Navigation Structure
```
Application Name
├── [Main Menu Item 1]
│   ├── [Submenu if needed]
│   └── [Submenu if needed]
├── [Main Menu Item 2]
├── [Main Menu Item 3]
└── Logout
```

## Entity Relationships
```
[Describe or diagram the relationships between main entities]
Example:
Organization (1) ──── (*) Tenant
Tenant (1) ──── (*) User
```

## Business Logic & Workflows

### Workflow: [Name]
1. [Step 1 description]
2. [Step 2 description]
3. [Validation points]
4. [Success/failure paths]

## Testing Requirements
- [Unit test expectations]
- [Integration test scenarios]
- [User acceptance criteria]

## Performance Considerations
- [Expected data volumes]
- [Concurrent user expectations]
- [Response time requirements]
- [Pagination needs for large datasets]

## Future Enhancements (Optional)
- [Planned features not in initial scope]
- [Potential integrations]
- [Scalability considerations]

## Development Notes
- [Any special instructions for developers]
- [Known limitations or workarounds]
- [Environment-specific configurations]

---

## How to Use This Template

1. **Replace all placeholders** in square brackets with your specific requirements
2. **Remove sections** that don't apply to your application
3. **Add sections** for any unique aspects of your application
4. **Keep examples** that help clarify requirements
5. **Update regularly** as requirements evolve

## Tips for Working with Claude

When using this template with Claude:
1. Fill in as much detail as possible before starting
2. Specify your preferences clearly (e.g., language choice, code style)
3. Include example data or scenarios
4. Reference existing code patterns you want to follow
5. Be explicit about what should NOT be done

## Related Documentation
- `Kiss-Analysis.md` - Complete KISS framework reference
- `Application.md` - Your specific application implementation
- Framework documentation in `/manual/` directory

---

*Template Version: 1.0*
*Created for: KISS Framework Applications*
*Last Updated: 2025-08-06*